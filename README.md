# Wiki-Search
HS Information Retrieval WS16/17 - Assignment 1


Simple search engine for a set of German Wikipedia pages.

Creates inverted indices & uses a boolean model to query search requests.



**Usage:**
- 1.1 creating indices
```
./create-index arg1 arg2
  arg1: INPUT - wiki text file in CONLL-X format
  arg2: OUTPUT - file in which inverted indices will be written
```
  
- 1.2 query indices

```
./query-index arg1 [OPTION]
  arg1: INPUT1 - produced text file of inverted indices
  [OPTION] - text file with doc id - title mapping
```
