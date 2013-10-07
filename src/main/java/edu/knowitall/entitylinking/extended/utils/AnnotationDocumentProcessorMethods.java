package edu.knowitall.entitylinking.extended.utils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import edu.knowitall.collection.immutable.Interval;
import edu.stanford.nlp.dcoref.CorefChain;
import edu.stanford.nlp.dcoref.CorefChain.CorefMention;
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefChainAnnotation;
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefClusterIdAnnotation;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.ling.CoreAnnotations.NamedEntityTagAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.TokensAnnotation;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.util.CoreMap;
import edu.knowitall.entitylinking.extended.utils.NamedEntityCollection;

public class AnnotationDocumentProcessorMethods {
	
	
	private static AnnotationDocumentProcessorMethods instance = null;
	protected AnnotationDocumentProcessorMethods(){
		
	}
	public static AnnotationDocumentProcessorMethods getInstance(){
		if(instance == null){
			instance = new AnnotationDocumentProcessorMethods();
		}
		return instance;
	}
	
	
	/**
	 * Given the information from a CorefMention determine the byte offsets
	 * of the whole mention and return as a knowitall Interval.
	 * @param document
	 * @param sentNum
	 * @param startIndex
	 * @param endIndex
	 * @return
	 */
	private Interval getCharIntervalFromCorefMention(Annotation document, Integer sentNum, Integer startIndex, Integer endIndex){
		
		List<CoreMap> sentences = document.get(SentencesAnnotation.class);
		CoreMap sentence = sentences.get(sentNum-1);
		List<CoreLabel> tokens = sentence.get(TokensAnnotation.class);
		List<CoreLabel> spanningTokens = new ArrayList<CoreLabel>();
		for(int i = startIndex; i < endIndex; i++){
			spanningTokens.add(tokens.get(i-1));
		}
		
		return Interval.closed(spanningTokens.get(0).beginPosition(),spanningTokens.get(spanningTokens.size()-1).endPosition());
		
	}
	
	public String getNamedEntityType(Annotation doc, int begOffset, int endOffset){
		
		List<CoreMap> sentences = doc.get(SentencesAnnotation.class);
	    for(CoreMap sentence: sentences){
	    	for(CoreLabel token: sentence.get(TokensAnnotation.class)){
	    		if((token.beginPosition() >= begOffset) && (token.endPosition() <= endOffset)){
	    			String net = token.get(NamedEntityTagAnnotation.class);
	    			System.out.println(net);
	    			if((!net.equals("ORGANIZATION")) && (!net.equals("PERSON")) && (!net.equals("LOCATION"))){
	    				net = "None";
	    			}
	    			return net;

	    		}
	    	}
	    }
	    return "None";
		
	}
	
	public List<Interval> getCorefIntervals(Annotation doc, int begOffset, int endOffset) {
		
		Map<Integer, CorefChain> graph = doc.get(CorefChainAnnotation.class);
		Integer corefClusterID = null;
		List<CoreMap> sentences = doc.get(SentencesAnnotation.class);
		
	    for(CoreMap sentence: sentences){
	    	for(CoreLabel token: sentence.get(TokensAnnotation.class)){
	    		if((token.beginPosition() >= begOffset) && (token.beginPosition() < endOffset)){
	    			if(corefClusterID == null){
	    				corefClusterID = token.get(CorefClusterIdAnnotation.class);
	    			}
	    		}
	    	}
	    }
	    if(corefClusterID != null){
	    	
	    	List<CorefMention> listOfCorefMentions = graph.get(corefClusterID).getMentionsInTextualOrder();
			if(listOfCorefMentions == null){
				  return new ArrayList<Interval>();	
				}
			else{
	    	List<Interval> offsets = new ArrayList<Interval>();
	    	for(CorefMention m : listOfCorefMentions){
	    		offsets.add(getCharIntervalFromCorefMention(doc,m.sentNum,m.startIndex,m.endIndex));
	    	}
	    	return offsets;
			}
	    }
	    else{
	        return new ArrayList<Interval>();
	    }
	}
	
	
	public NamedEntityCollection getNamedEntities(Annotation doc) throws Exception{		
		
		List<CoreMap> sentences = doc.get(SentencesAnnotation.class);
    	List<List<CoreLabel>> allTokens = new ArrayList<List<CoreLabel>>();
    	List<CoreLabel> relevantTokens = new ArrayList<CoreLabel>();
    	int sentIndex =0;
	    for(CoreMap sentence: sentences){
	    	List<CoreLabel> sentenceTokenList = new ArrayList<CoreLabel>();
	    	int tokenIndex =0;
	    	for(CoreLabel token: sentence.get(TokensAnnotation.class)){
	    			String net = token.get(NamedEntityTagAnnotation.class);
    				token.setIndex(tokenIndex);
    				token.setSentIndex(sentIndex);		    			
	    			if( (net.equals("ORGANIZATION"))||
	    				(net.equals("LOCATION")) ||
	    				(net.equals("PERSON"))
	    				){
	    				relevantTokens.add(token);
	    			}
	    			sentenceTokenList.add(token);
	    		tokenIndex +=1 ;
	    	}
	    	allTokens.add(sentenceTokenList);
	    	sentIndex += 1;
	    }
	    
    	if(!relevantTokens.isEmpty()){
    		
	    	List<List<CoreLabel>> matchingTypes = new ArrayList<List<CoreLabel>>();
	    	List<CoreLabel> firstTokenList = new ArrayList<CoreLabel>();
	    	firstTokenList.add(relevantTokens.get(0));
	    	matchingTypes.add(firstTokenList);
	    	relevantTokens.remove(0);
	    	for(CoreLabel t : relevantTokens){
	    		int currIndex = matchingTypes.size()-1;
	    		List<CoreLabel> lastTokenList = matchingTypes.get(currIndex);
	    		CoreLabel lastToken = lastTokenList.get(lastTokenList.size()-1);
	    		
	    		if((t.sentIndex() == lastToken.sentIndex()) 
	    				&&  (t.index() == (1 + lastToken.index())) &&
	    				(t.ner().equals(lastToken.ner()))){
	    			matchingTypes.get(currIndex).add(t);
	    		}
	    		else if((t.ner().equals("LOCATION") && lastToken.ner().equals("LOCATION")) &&
	    				(t.sentIndex()== lastToken.sentIndex()) && (t.index() == (2 + lastToken.index())) && 
	    				(allTokens.get(t.sentIndex()).get(t.index()-1).originalText().equals(",") ||
	    				 allTokens.get(t.sentIndex()).get(t.index()-1).originalText().equals("in"))){
	    			matchingTypes.get(currIndex).add(allTokens.get(t.sentIndex()).get(t.index()-1));
	    			matchingTypes.get(currIndex).add(t);
	    		}
	    		else{
	    			List<CoreLabel> newTokenList = new ArrayList<CoreLabel>();
	    			newTokenList.add(t);
	    			matchingTypes.add(newTokenList);
	    		}
	    	}
	    	
	    	List<String> locations = new ArrayList<String>();
	    	List<String> organizations = new ArrayList<String>();
	    	List<String> people = new ArrayList<String>();

	    	for(List<CoreLabel> namedEntity : matchingTypes){
	    		StringBuilder sb = new StringBuilder();
	    		for(CoreLabel t : namedEntity){
	    			sb.append(" ");
	    			sb.append(t.originalText());
	    		}
	    		String namedEntityString = sb.toString().trim();
	    		String namedEntityType = namedEntity.get(0).ner();
	    		switch (namedEntityType){
	    			case "ORGANIZATION" : {
	    				organizations.add(namedEntityString);
	    				break;
	    			}
	    			case "LOCATION" : {
	    				locations.add(namedEntityString);
	    				break;
	    			}
	    			case "PERSON" : {
	    				people.add(namedEntityString);
	    				break;
	    			}
	    			default : {
	    				throw new Exception("All namedEntities should be ORGANIZATION, LOCATION, or PERSON");
	    			}
	    		}
	    	}
	    	return new NamedEntityCollection(scala.collection.JavaConversions.asScalaIterable(organizations).toList(),
	    									scala.collection.JavaConversions.asScalaIterable(locations).toList(),
	    									scala.collection.JavaConversions.asScalaIterable(people).toList());
    	}
    	else{
    		return null;
    	}				
	}
	
	

}
