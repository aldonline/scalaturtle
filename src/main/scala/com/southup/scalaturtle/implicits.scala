package com.southup.scalaturtle

import org.openrdf.model.URI
import org.openrdf.model.impl.ValueFactoryImpl

object implicits {
  private def vf = ValueFactoryImpl.getInstance()
  implicit def string2rdfstringops( str:String ):RDFStringOps = new RDFStringOps( str )
  class RDFStringOps( val str:String ){
    def uri = vf.createURI(str)
    def lit = vf.createLiteral( str )
    def lang(langTag:String) = vf.createLiteral( str, langTag )
    def typed(xsdt:URI) = vf.createLiteral( str, xsdt )
  }  
}