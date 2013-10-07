package edu.knowitall.entitylinking.extended.utils

case class NamedEntityCollection(
      val organizations: List[String],
      val locations: List[String],
      val people: List[String])