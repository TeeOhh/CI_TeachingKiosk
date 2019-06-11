# CI_TeachingKiosk
Repo for Conversational Interfaces group "Teaching the Kiosk". Home of our wonderful prototypes: ontology of NU course, people, etc., a rich graph of computer science sub areas, and rules for reasoning about all of the former to build user models and provide useful information to Friends of the Kiosk.

## Getting Started

These instructions will get you a copy of the system on your local machine for testing purposes. See deployment for notes on how to deploy the project on a live system.

### Contents

What resides within...

* demo.lsp
* krf/rules.krf
* MORE FILES

### Installing

How to get a development env running and some more examples to test that you have done so correctly.

* Open Allegro Common Lisp
* Compile and load load.lsp
* Load the krf files. Running this method will load the necessary knowledge into the KB for reasoning:
  ```
  (load-kiosk-mts)
  ```
  
Then test if the knowledge was loaded correctly with a simple query:
```
(load-kiosk-mts)
```
If loading was successful, the query should return:
 ```
OUTPUT
 ```

## More example queries and their outputs

```
Query 1
```

```
Output 1
```

```
Query 2
```

```
Output 2
```

```
Query 3
```

```
Output 3
```

```
Query 4
```

```
Output 4
```

```
Query 5
```

```
Output 5
```

## Deployment for Kiosk

* Load knowledge and rules into KB that is deployed on Kiosk
* With added training questions to analogical QA and added support to EA, questions like "Who teaches a course in a topic I like?" or "Who is an expert in Artificial Intelligence?" should be able to be answered

## Built With

* Allegro Common Lisp (Modern) 10.1
* Companions Architecture
  * FIRE
  * NextKB
* Python3

## Authors

* **Taylor Olson**

* **Kapil Garg**

* **Yihong Hu**
