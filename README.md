# Conversational Interfaces: Teaching the Kiosk
Repository for Conversational Interfaces group "Teaching the Kiosk". All project work was conducted in Spring 2019.

This repository contains :
1. Code to scrape and ontologize the following:
    1. NU Computer Science courses, faculty, and research groups
    2. Events occurring around campus
    3. A graph of topics and sub-topics in Computer Science
2. Rules for reasoning about all of the former to build user models and provide useful information to Friends of the Kiosk.

## Repository Structure
```
.
├── README.md
├── Pipfile      // Pipenv Python env file to use for scraping
├── Pipfile.lock // Pipenv file 
├── data         // Raw data used for scraping (courses, groups)
├── documents    // PDFs of graphs used during presentations
├── json         // Scraped data as JSON (only ACM CCS was done)
├── krf          // All predicates and rules
├── load.lsp     // Loader for krf files to FIRE
├── scraping     // Scraping code
└── tree_viz     // Simple visualization for ACM CCS
```

## Getting Started

These instructions will get you a copy of the system on your local machine for testing purposes. See deployment for notes on how to deploy the project on a live system.

### Setting Up and Running Scraping Code
1. Make sure you have [Python 3.7](https://www.python.org/downloads/release/python-370/) and [Pipenv](https://docs.pipenv.org/en/latest/) installed.
2. Run `pipenv install` to install needed dependencies and `pipenv shell` to start a virtual environment.
3. Navigate to `scraping/`.
4. To run all scraping code and generate krf files, run `python scrape_all.py`. Alternatively, run `python <filename>.py` to scrape for a particular dataset.
5. Navigate to `krf/` to see scraped output as krf files.

### Setting Up the KB and Making Queries

How to get a development env running and some more examples to test that you have done so correctly.
1. Open Allegro Common Lisp
2. Compile and load load.lsp
3. Load the krf files. Running this method will load the necessary knowledge into the KB for reasoning:
    ```
    (load-kiosk-mts)
    ```
4. Test if the knowledge was loaded correctly with a simple query:
    ```
    (load-kiosk-mts)
    ```
    If loading was successful, the query should return:
    ```
    OUTPUT
    ```

## Example Queries and Outputs

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
These are some high-level instructions for how to deploy and use our code on the Kiosk.
1. Load knowledge and rules into KB that is deployed on Kiosk
2. With added training questions to analogical QA and added support to EA, questions like "Who teaches a course in a topic I like?" or "Who is an expert in Artificial Intelligence?" should be able to be answered

## Tools Used
* Allegro Common Lisp (Modern) 10.1
* Companions Architecture
  * FIRE
  * NextKB
* Python 3.7

## Contributors

* [Taylor Olson](https://github.com/TeeOhh)

* [Kapil Garg](https://github.com/kapil1garg)

* [Yihong Hu](https://github.com/Huhuhu812)
