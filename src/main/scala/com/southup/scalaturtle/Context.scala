package  com.southup.scalaturtle

import scala.collection.mutable.HashSet
import org.openrdf.model._
import org.openrdf.model.impl.ValueFactoryImpl

sealed abstract class Step[T <: Resource]( ctx:Context ) {
  def vf = ctx.vf
  def subject:T
}

trait AnyObjectHandler[T <: Resource] {
  def vf:ValueFactory // abstract
  def ~(o:Value):StepObject[T] // abstract
  // TODO: complete java 2 rdf conversions
  private def any2val( x:Any ):Value = x match {
    case v:Value => v
    case v:Long => vf.createLiteral(v)
    case v:Int => vf.createLiteral(v)
    case v:String => vf.createLiteral(v)
  }
  def ~(o:Any):StepObject[T] = this.~(any2val(o)) 
}

class StepSubject[T <: Resource]( val s:T, ctx:Context ) extends Step[T](ctx) {
  def ~( p:URI ):StepPredicate[T] = new StepPredicate[T]( this, p, ctx )
  def subject = s
}

class StepPredicate[T <: Resource]( val prev:StepSubject[T], val p:URI, ctx:Context ) 
    extends Step[T](ctx) with AnyObjectHandler[T] {
  def ~( o:Value ):StepObject[T] = new StepObject[T]( this, o, ctx )
  def subject = prev.subject
}

class StepObject[T <: Resource]( val prev:StepPredicate[T], val o:Value, ctx:Context )
    extends Step[T](ctx) with AnyObjectHandler[T] {
  ctx.assert( vf.createStatement(prev.prev.s, prev.p, o ) )
  def ~~~[M <: Resource]( s:M ):StepSubject[M] = new StepSubject[M]( s, ctx )
  def ~~( p:URI ):StepPredicate[T] = new StepPredicate[T]( prev.prev, p, ctx )
  def ~( o:Value ):StepObject[T] = new StepObject[T]( prev, o, ctx )
  def subject = prev.subject
  def unary_~ = subject
}

class Context {
  private val _statements = new HashSet[Statement]
  def statements:Set[Statement] = _statements.toSet
  def assert(st:Statement) = _statements += st
  lazy val vf = ValueFactoryImpl.getInstance()
  lazy val implicits = new ContextImplicits( Context.this )
}

class ContextImplicits ( ctx:Context ) {
  implicit def resource2stepsubject[T <: Resource]( r:T ):StepSubject[T] = new StepSubject[T]( r, ctx )
  implicit def string2rdfstringops( str:String ):implicits.RDFStringOps = implicits.string2rdfstringops( str:String )
}

