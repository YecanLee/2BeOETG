## Towards Better Open-Ended Text Generation: A Multicriteria Evaluation Framework<br><sub>Official Implementation</sub>

### [Paper](future_archieve_paper_link) | [Project Page](https://yecanlee.github.io/2beoetgwebsite.github.io/) | Run Analysis Baseline [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/drive/1C6u0i0fHpk80uPhDWNylIlq64F_HSl-1?usp=sharing)
<img src="assets/benchmark_art.png" alt="A symbol of multi-criteria evaluation" width="300" />

This repo contains the official implementation of our paper __"Towards Better Open-Ended Text Generation: A Multicriteria Evaluation Framework"__. You can find more details in our [project page](https://yecanlee.github.io/2beoetgwebsite.github.io/) and our [paper](future_archieve_paper_link).

> [**Towards Better Open-Ended Text Generation: A Multicriteria Evaluation Framework**](future_archieve_paper_link)<br>
> [Esteban Garces Arias](https://scholar.google.com/citations?user=FK1UX0gAAAAJ&hl=es),[Hannah Blocher](https://www.foundstat.statistik.uni-muenchen.de/personen/mitglieder/blocher/index.html), [Julian Rodemann](https://rodemann.github.io/), [Meimingwei Li](https://github.com/YecanLee), [Christian Heumann](https://scholar.google.de/citations?user=H6LdyzoAAAAJ&hl=de),[Matthias Aßenmacher](https://www.slds.stat.uni-muenchen.de/people/assenmacher/)
> <br>Department of Statistics, LMU Munich, Munich Center for Machine Learning (MCML)<br>

In this paper, we present novel ranking strategies within this multicriteria framework. Specifically, we employ benchmarking approaches based on partial orderings and present a new summary metric designed to balance existing automatic indicators, providing a more holistic evaluation of text generation quality. Furthermore, we discuss the alignment of these approaches with human judgments. Our experiments demonstrate that the proposed methods offer a robust way to compare decoding strategies, exhibit similarities with human preferences, and serve as valuable tools in guiding model selection for open-ended text generation tasks. Finally, we suggest future directions for improving evaluation methodologies in text generation. 

This repository contains:

* 🪐 A simple R [implementation](future_BRT_model.R) of Extended Bradley-Terry model
* ⚡️ Faster Metrics Calculation with [Coherence](coherence_compute.py), [MAUVE and Diversity](diversity_compute.py) in Python
* 💥 A [Colab notebook](https://colab.research.google.com/drive/1C6u0i0fHpk80uPhDWNylIlq64F_HSl-1?usp=sharing) for running qstar analysis Demo in colab

## Table of Contents 📖  <a href="#top">[Back to Top]</a>

- [Setup Environment](#Setup-Environment-)
- [Data Download](#Data-Download-)
- [Run Analysis Baseline](#Run-Analysis-Baseline-)
- [Contributions](#Contributions-)

## Setup Environment 💻 <a href="#top">[Back to Top]</a> <a name="Setup-Environment"></a>

To install all the dependencies for this repo, run the following command:
```bash
pip install -r requirements.txt
SKLEARN_ALLOW_DEPRECATED_SKLEARN_PACKAGE_INSTALL=True pip install simctg
```

We recommend you to build a new conda environment to use the repository.

```bash
conda create -n helmet python=3.11
conda activate helmet
pip install -r requirements.txt
SKLEARN_ALLOW_DEPRECATED_SKLEARN_PACKAGE_INSTALL=True pip install simctg
```

## Data Download 📚 <a href="#top">[Back to Top]</a> <a name="Data-Download"></a>
To download the data, please run the following command:
```bash
bash download_data.sh
```

## Run Analysis Baseline 🔥 <a href="#top">[Back to Top]</a> <a name="Run-Analysis-Baseline"></a>
We open-sourced our pre-computed LLM inference results to reproduce our paper's analysis results. Please refer to the [Data Download](#Data-Download-) section to download the data. If you want to use your own inference results, please follow the following instructions.

### Use Customized Inference Results.
#### Run coherence computation 
To run the coherence computation for your own inference results json file, please run the following command:
```bash
python coherence_computation.py \
--opt_model_name $OPT_MODEL_NAME \
--test_path $TEST_PATH
```

#### Run diversity computation
To run the diversity computation for your own inference results json file, please run the following command:
```bash
python diversity_computation.py \
--test_path $TEST_PATH
```

### Use pre-computed results
#### Run qstar analysis
To run the qstar analysis, please run the following command:
```bash
Rscript qstar_metric.R
```
You may need to modify the following lines in order to fit your data path:
```R
# The following line locates at line 14, please change it to your results_with_pareto_efficiency.csv path
data_file <- "path/to/results_with_pareto_efficiency.csv"
# The following line locates at line 15, please change it to the path you want to save the 'ranking_qtext.csv'
candidate_stats_file <- "path/to/ranking_qtext.csv"
# The following line locates at line 16, please change it to the path you want to save the 'dominance_final_analysis.csv'
dominance_summary_file <- "path/to/dominance_final_analysis.csv"
```

## Contributions 🚀 <a href="#top">[Back to Top]</a> <a name="Contributions"></a>

This repository is based on the following repositories:
- [Contrastive Search versus Contrastive Decoding](https://github.com/yxuansu/Contrastive_Search_versus_Contrastive_Decoding)
- [Adaptive Contrastive Search: Uncertainty-Guided Decoding for Open-Ended Text Generation](https://github.com/YecanLee/Adaptive_Contrastive_Search)

We thank the authors for their open-sourced code.