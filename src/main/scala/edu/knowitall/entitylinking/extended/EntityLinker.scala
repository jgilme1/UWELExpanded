package edu.knowitall.entitylinking.extended

import edu.stanford.nlp.pipeline.Annotation
import edu.knowitall.entitylinking.extended.utils.AnnotationDocumentProcessorMethods
import edu.knowitall.entitylinking.extended.utils.StanfordAnnotatorHelperMethods
import org.apache.commons.io.FileUtils
import java.io.File
import edu.knowitall.entitylinking.extended.utils.HelperMethods

object EntityLinker {

  def link(doc: Annotation, begOffset: Integer,endOffset: Integer,docText: String): String = {
    
    //do an error check for doc and docText consistency
    
    val annotatedDocProcessorMethods = AnnotationDocumentProcessorMethods.getInstance()
    val namedEntities = annotatedDocProcessorMethods.getNamedEntities(doc)
    val corefOffsets = scala.collection.JavaConversions.asScalaIterable(annotatedDocProcessorMethods.getCorefIntervals(doc, begOffset, endOffset))
    for(co <- corefOffsets){
      println(co)
    }
    val namedEntityType = annotatedDocProcessorMethods.getNamedEntityType(doc,begOffset,endOffset)
    println("NE Type = " + namedEntityType)
    
    val bestEntityMention = HelperMethods.identifyBestEntityStringByRules(namedEntities,namedEntityType,corefOffsets,begOffset,endOffset,docText)
    println("Best entity mention = " +bestEntityMention)
    
    val context = HelperMethods.getCorefContext(corefOffsets,doc,begOffset)
    println("Context = ") 
    for(con <- context){
      println(con)
    }
    "s"
  }

  
  def main(args: Array[String]){
    val stanfordHelper = new StanfordAnnotatorHelperMethods(true)
    val docText = FileUtils.readFileToString(new File(args(0)))
    val doc = new Annotation(docText)
    stanfordHelper.annotateDocument(doc)
    link(doc,2367,2374,docText)
    link(doc,2367,2375,docText)
  }
}