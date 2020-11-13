import java.util.stream.Stream

import org.semanticweb.owlapi.model.parameters.ChangeApplied
import org.semanticweb.owlapi.model.{
  IRI,
  OWLAxiom,
  OWLClass,
  OWLClassAssertionAxiom,
  OWLClassExpression,
  OWLDataMaxCardinality,
  OWLDataProperty,
  OWLDataPropertyExpression,
  OWLDisjointClassesAxiom,
  OWLDisjointUnionAxiom,
  OWLEquivalentClassesAxiom,
  OWLIndividual,
  OWLNamedIndividual,
  OWLObjectComplementOf,
  OWLObjectIntersectionOf,
  OWLObjectInverseOf,
  OWLObjectMaxCardinality,
  OWLObjectProperty,
  OWLObjectPropertyExpression,
  OWLObjectUnionOf,
  OWLOntology,
  OWLOntologyManager
}

case class OwlApi(manager: OWLOntologyManager) {

  val factory = manager.getOWLDataFactory

  def addAxiom(o: OWLOntology, a: OWLAxiom): ChangeApplied =
    manager.addAxiom(o, a)

  def getOWLThing: OWLClass = factory.getOWLThing

  def getOWLNothing: OWLClass = factory.getOWLNothing

  def getOWLClass(iri: IRI): OWLClass = factory.getOWLClass(iri)

  def getOWLDataProperty(iri: IRI): OWLDataProperty =
    factory.getOWLDataProperty(iri)

  def getOWLObjectProperty(iri: IRI): OWLObjectProperty =
    factory.getOWLObjectProperty(iri)

  def getOWLNamedIndividual(iri: IRI): OWLNamedIndividual =
    factory.getOWLNamedIndividual(iri)

  def getOWLObjectInverseOf(property: OWLObjectProperty): OWLObjectInverseOf =
    factory.getOWLObjectInverseOf(property)

  def getOWLClassAssertionAxiom(ce: OWLClassExpression,
                                i: OWLIndividual): OWLClassAssertionAxiom =
    factory.getOWLClassAssertionAxiom(ce, i)

  def getOWLObjectComplementOf(e: OWLClassExpression): OWLObjectComplementOf =
    factory.getOWLObjectComplementOf(e)

  def getOWLObjectIntersectionOf(
      operands: Stream[OWLClassExpression]): OWLObjectIntersectionOf =
    factory.getOWLObjectIntersectionOf(operands)

  def getOWLObjectUnionOf(
      operands: Stream[OWLClassExpression]): OWLObjectUnionOf =
    factory.getOWLObjectUnionOf(operands)

  def getOWLDisjointClassesAxiom(
      operands: Stream[OWLClassExpression]): OWLDisjointClassesAxiom =
    factory.getOWLDisjointClassesAxiom(operands)

  def getOWLEquivalentClassesAxiom(
      operands: Stream[OWLClassExpression]): OWLEquivalentClassesAxiom =
    factory.getOWLEquivalentClassesAxiom(operands)

  def getOWLDisjointUnionAxiom(
      c: OWLClass,
      operands: Stream[OWLClassExpression]): OWLDisjointUnionAxiom =
    factory.getOWLDisjointUnionAxiom(c, operands)

  def getOWLDataMaxCardinality(
      cardinality: Int,
      pe: OWLDataPropertyExpression): OWLDataMaxCardinality =
    factory.getOWLDataMaxCardinality(cardinality, pe)

  def getOWLObjectMaxCardinality(
      cardinality: Int,
      pe: OWLObjectPropertyExpression): OWLObjectMaxCardinality =
    factory.getOWLObjectMaxCardinality(cardinality, pe)
}
