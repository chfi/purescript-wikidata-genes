## purescript-wikidata-genes

An embeddable app that queries the Wikidata SPARQL service to
provide information on genes, their names, and alternative aliases.

The SPARQL queries are constructed using a very simple DSL, which
may be expanded upon and factored out into a `purescript-sparql`
library in the future.


### SPARQL queries

There are essentially four questions that this tool uses Wikidata to
answer, alongside some auxiliary queries to provide more user-friendly
details.

1. HomologeneID -> Taxon* AND Gene*
2. Gene* -> HomologeneID
3. Gene* -> GeneName AND GeneAliases
4. Taxon* and GeneName OR GeneAlias -> HomologeneID

The * suffix is used to signify a Wikidata entry (URI, really);
otherwise the result is a literal, probably a string.

The important thing to remember is that the only potentially tricky
part is "entering" the graph at the right place, given e.g. a gene
label or alias, and the scientific name of a species. One might feel
inclined to grab the HomologeneID directly, using those pieces of
data, but it is much easier to work with things if one simply
identifies the relevant entry in Wikidata first. With that in hand,
traversing the graph to find what one is looking for becomes very
easy, as you only need to follow the arrows.

#### HomologeneID -> Taxon and Gene

Given a HomoloGene ID (in this case 22758, ADD1), return each
corresponding Taxon and Gene.

```sparql
SELECT ?gene ?tax_id
WHERE
{
	?gene wdt:P31 wd:Q7187 ; # ?gene instance of gene
          wdt:P593 "22758" ; # ?gene homologeneID is 22758
          wdt:P703 ?tax_id . # ?gene is found in NCBI taxonomy id ?tax_id
}
```

#### Gene -> HomologeneID

A wikidata gene entry is specifically a gene in some species,
and it should have a direct link to its HomologeneID.

```sparql
SELECT ?homologeneID
WHERE
{
    # ?homologeneID for Add1 in Mus musculus
    wd:Q18247422 wdt:P593 ?homologeneID .
}
```

#### Gene -> Gene name and aliases

(unsure about this one, actually)

```sparql
SELECT ?geneLabel ?geneAltLabel
WHERE
{
    ?gene = <gene>
}
```

#### Auxiliary queries


```sparql
```

##### Taxon -> Taxon name and common name

```sparql
SELECT ?taxonName ?taxonCommon
WHERE
{
    ?taxon wdt:P225 ?taxonName;
           wdt:P1843 ?taxonCommon.

    FILTER(LANG(?taxonCommon) = "en").
}
```

##### Taxon name or common name -> Taxon

Taxon name -> Taxon

In this case, taxon name is "mus musculus"
```sparql
SELECT ?taxon
WHERE
{
    ?taxon wdt:P225 ?taxonName.
    FILTER(LCASE(?taxonName) = "mus musculus").
}
```

Taxon common (English) name -> Taxon

In this case, taxon common name is "house mouse"

```sparql
SELECT ?taxon
WHERE
{
    ?taxon wdt:P1843 ?taxonCommon.
    FILTER(LANG(?taxonCommon) = "en" &&
           LCASE(STR(?taxonCommon)) = "house mouse").
}
```

##### Taxon and gene (name or alias) -> Gene

Given a Wikidata entry for some taxon, and the name or alias of some
gene, find the corresponding HomologeneID.

In this case, taxon is "Mus musculus", wd:Q83310 and gene label is "Add1".

```sparql
SELECT ?homologeneID
WHERE
{
    ?gene wdt:P31 wd:Q7187 ;
          rdfs:label ?geneLabel ;
          wdt: wd:Q83310 .

    FILTER(STR(?geneLabel) = "Add1" &&
           LANG(?geneLabel) = "en").
}
```

In this case, taxon name is "Mus musculus", wd:Q83310 and gene alias is "AI256389".

```sparql
SELECT ?homologeneID
WHERE
{
    ?gene wdt:P31 wd:Q7187 ;
          skos:altLabel ?geneAltLabel ;
          wdt: wd:Q83310 .

    FILTER(STR(?geneAltLabel) = "AI256389" &&
           LANG(?geneAltLabel) = "en").
}
```
