;;;-*- Mode: common-lisp; syntax: common-lisp; package: cl; base: 10 -*-
;;;
;;;; Packages in RDF, RDFS, OWL, and other basic ontologies in Semantic Webs
;;;
;;; This module defines basic symbols in rdf, rdfs, and owl package.
;;; This module also includes dc, dcterms, dcam, dctype, skos, and grddl package.
;;; Those symbols are exported so as to be QNames.
;;;
;;; ----------------------------------------------------------------------------------
;;; Copyright (c) 2014, Seiji Koide <seijikoide0@gmail.com>
;;; Permission is hereby granted, free of charge, to any person obtaining a 
;;; copy of this software and associated documentation files (the "Software"), 
;;; to deal in the Software without restriction, including without limitation 
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense, 
;;; and/or sell copies of the Software, and to permit persons to whom the 
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included 
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------
;;; 
;;; The views and conclusions contained in the software and documentation are those
;;; of the authors and should not be interpreted as representing official policies, 
;;; either expressed or implied, of the FreeBSD Project.
;;
;; History
;; -------
;; 2020/08/29   xml and xsd package is moved to xmlreader.lisp
;;

(cl:provide :basics-packages)




#|
(cl:defpackage :rdf
  (:use ) ; supressing using common lisp package
  (:export ;; RDF 1.1
   "HTML" "langString" "PlainLiteral" "type" "Property" "Statement" 
   "subject" "predicate" "object" "Bag" "Seq" "Alt" "value" 
   "List" "nil" "first" "rest" "XMLLiteral"
   ;; RDF parser
   "XMLDatatype" #:inLang 
   "li" "XMLDecl" "Description" "XMLLiteral-equal"
   "_1" "_2" "_3" "_4" "_5" "_6" "_7" "_8" "_9"
   ;; Utils
   #:type-p #:subclass-p
   ;; oldTerms
   "aboutEach" "aboutEachPrefix" "bagID"
   ;; coreSyntaxTerms
   "RDF" "ID" "about" "parseType" "resource" "nodeID" "datatype"
   )
  (:documentation "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))

;; OWL 2 MOF model
(cl:defpackage :rdf
  (:use ) ; supressing using common lisp package
  (:export "langRange"
           ))
   
(cl:defpackage :rdfs
  (:use ) ; supressing using common lisp package
  (:export "Resource" "Class" "subClassOf" "subPropertyOf" "comment" "label" 
           "domain" "range" "seeAlso" "isDefinedBy" "Literal" 
           "Container" "ContainerMembershipProperty" "member" "Datatype")
  (:documentation "http://www.w3.org/2000/01/rdf-schema#"))

(defpackage :owl
  (:use ) ; supressing using common lisp package
  (:export
   "allValuesFromRestriction" "someValuesFromRestriction" "hasValueRestriction" "cardinalityRestriction" 
   "describe-slot-constraint" "Thing"
   ;; OWL 2
   "AllDifferent" "AllDisjointClasses" "AllDisjointProperties" "Annotation" 
   "AnnotationProperty" "AsymmetricProperty" "Axiom" "Class" "DataRange" "DatatypeProperty" 
   "DeprecatedClass" "DeprecatedProperty" "FunctionalProperty" "InverseFunctionalProperty" 
   "IrreflexiveProperty" "NamedIndividual" "NegativePropertyAssertion" "Nothing" "ObjectProperty" 
   "Ontology" "OntologyProperty" "ReflexiveProperty" "Restriction" "SymmetricProperty" 
   "TransitiveProperty" "Thing" "allValuesFrom" "annotatedProperty" "annotatedSource" "annotatedTarget" 
   "assertionProperty" "backwardCompatibleWith" "bottomDataProperty" "bottomObjectProperty" 
   "cardinality" "complementOf" "datatypeComplementOf" "deprecated" "differentFrom" 
   "disjointUnionOf" "disjointWith" "distinctMembers" "equivalentClass" "equivalentProperty" 
   "hasKey" "hasSelf" "hasValue" "imports" "incompatibleWith" "intersectionOf" "inverseOf" 
   "maxCardinality" "maxQualifiedCardinality" "members" "minCardinality" "minQualifiedCardinality" 
   "onClass" "onDataRange" "onDatatype" "oneOf" "onProperties" "onProperty" "priorVersion" 
   "propertyChainAxiom" "propertyDisjointWith" "qualifiedCardinality" "sameAs" "someValuesFrom" 
   "sourceIndividual" "targetIndividual" "targetValue" "topDataProperty" "topObjectProperty" 
   "unionOf" "versionInfo" "versionIRI" "withRestrictions"
   )
  (:documentation "http://www.w3.org/2002/07/owl#")
  )

;; OWL 2 MOF model
(defpackage :owl
  (:use ) ; supressing using common lisp package
  (:export "rational" "real"))

(cl:defpackage :dc
  (:use ) ; supressing using common lisp package
  (:export "contributor" "coverage" "creator" "date" "description" "format" "identifier" 
           "language" "publisher" "relation" "rights" "source" "subject" "title" "type")
  (:documentation "http://purl.org/dc/elements/1.1/"))

(cl:defpackage :dcterms
  (:use ) ; supressing using common lisp package
  (:export
   "Agent" "AgentClass" "BibliographicResource" "Box" "DCMIType" "DDC" "FileFormat" "Frequency" 
   "IMT" "ISO3166" "ISO639-2" "ISO639-3" "Jurisdiction" "LCC" "LCSH" "LicenseDocument" "LinguisticSystem" 
   "Location" "LocationPeriodOrJurisdiction" "MESH" "MediaType" "MediaTypeOrExtent" "MethodOfAccrual" 
   "MethodOfInstruction" "NLM" "Period" "PeriodOfTime" "PhysicalMedium" "PhysicalResource" "Point" "Policy" 
   "ProvenanceStatement" "RFC1766" "RFC3066" "RFC4646" "RFC5646" "RightsStatement" "SizeOrDuration" 
   "Standard" "TGN" "UDC" "URI" "W3CDTF" "abstract" "accessRights" "accrualMethod" "accrualPeriodicity" 
   "accrualPolicy" "alternative" "audience" "available" "bibliographicCitation" "conformsTo" "contributor" 
   "coverage" "created" "creator" "date" "dateAccepted" "dateCopyrighted" "dateSubmitted" "description" 
   "educationLevel" "extent" "format" "hasFormat" "hasPart" "hasVersion" "identifier" "instructionalMethod" 
   "isFormatOf" "isPartOf" "isReferencedBy" "isReplacedBy" "isRequiredBy" "isVersionOf" "issued" "language" 
   "license" "mediator" "medium" "modified" "provenance" "publisher" "references" "relation" "replaces" 
   "requires" "rights" "rightsHolder" "source" "spatial" "subject" "tableOfContents" "temporal" "title" 
   "type" "valid")
  (:documentation "http://purl.org/dc/terms/"))

(cl:defpackage :dcam
  (:use ) ; supressing using common lisp package
  (:export "VocabularyEncodingScheme" "memberOf")
  (:documentation "http://purl.org/dc/dcam/"))

(cl:defpackage :dctype
  (:use ) ; supressing using common lisp package
  (:export "Collection" "Dataset" "Event" "Image" "InteractiveResource" "MovingImage" "PhysicalObject" 
           "Service" "Software" "Sound" "StillImage" "Text" )
  (:documentation "http://purl.org/dc/dcmitype/"))

(cl:defpackage :skos
  (:use ) ; supressing using common lisp package
  (:export
   "Concept" "ConceptScheme" "Collection" "OrderedCollection" "inScheme" "hasTopConcept" "topConceptOf" 
   "prefLabel" "altLabel" "hiddenLabel" "notation" "note" "changeNote" "definition" "editorialNote" 
   "example" "historyNote" "scopeNote" "semanticRelation" "broader" "narrower" "related" 
   "broaderTransitive" "narrowerTransitive" "member" "memberList" "mappingRelation" 
   "broadMatch" "narrowMatch" "relatedMatch" "exactMatch" "closeMatch")
  (:documentation "http://www.w3.org/2004/02/skos/core#"))

(cl:defpackage :grddl
  (:use ) ; supressing using common lisp package
  (:export "namespaceTransformation")
  (:documentation "http://www.w3.org/2003/g/data-view#"))
|#
