import json
import torch
import argparse
import numpy as np
from transformers import GPT2Tokenizer
from simctg.evaluation import measure_repetition_and_diversity

def decode(tokens, tokenizer):
    token_id_list = tokenizer.convert_tokens_to_ids(tokens)
    text = tokenizer.decode(token_id_list)
    return text

def parse_diversity_gen_len_text(text, tokenizer, prefix_len):
    tokens = tokenizer.tokenize(text)[prefix_len:]
    return decode(tokens, tokenizer)

def measure_generation_length(text):
    """Compute the number of words in the generated text."""
    return len(text.strip().split())

def load_result(in_f):
    with open(in_f, 'r') as f:
        result_list = json.load(f)

    prefix_text_list, prediction_text_dict, gold_text_list = [], {}, []
    for item in result_list:
        prefix_text_list.append(item['prefix_text'])
        gold_text_list.append(item['reference_text'])
        for key, gen_text in item['generated_result'].items():
            if key not in prediction_text_dict:
                prediction_text_dict[key] = []
            prediction_text_dict[key].append(gen_text)
    return prefix_text_list, prediction_text_dict, gold_text_list

def compute_one_gen_len(text):
    return measure_generation_length(text)

def parse_config():
    parser = argparse.ArgumentParser()
    parser.add_argument("--test_path", type=str, required=True, help="Path to the input JSON file.")
    return parser.parse_args()

if __name__ == '__main__':
    args = parse_config()
    evaluation_save_path = args.test_path.rstrip('.json') + '_diversity_mauve_gen_length_result.json'

    tokenizer = GPT2Tokenizer.from_pretrained('gpt2')
    prefix_text_list, prediction_text_dict, gold_text_list = load_result(args.test_path)

    number_of_predictions = len(prediction_text_dict)
    print(f'Number of predictions per instance: {number_of_predictions}')

    # Initialize lists to store per-component scores
    per_component_results = []

    for idx in range(len(prefix_text_list)):
        prefix = prefix_text_list[idx]
        gold = gold_text_list[idx]
        component_result = {
            'prefix_text': prefix,
            'reference_text': gold,
            'diversity_scores': [],
            'generation_lengths': []
        }

        generated_texts = []
        # Collect all generated texts for this component
        for key in sorted(prediction_text_dict.keys()):
            generated_text = prediction_text_dict[key][idx]
            generated_texts.append(generated_text)

            # Compute generation length
            gen_len = compute_one_gen_len(generated_text)
            component_result['generation_lengths'].append(gen_len)

        # Compute Diversity Score for this component
        # Diversity is computed across all generated texts for this component
        _, _, _, div_score = measure_repetition_and_diversity(generated_texts)
        component_result['diversity_scores'].append(round(div_score * 100, 2))

        per_component_results.append(component_result)

    # After processing all components, compute aggregate statistics
    # Extract all MAUVE and Diversity scores, excluding None
    all_diversity_scores = [comp['diversity_scores'][0] for comp in per_component_results if len(comp['diversity_scores']) > 0]
    all_gen_lengths = [length for comp in per_component_results for length in comp['generation_lengths']]

    aggregate_results = {
        'diversity_mean': round(np.mean(all_diversity_scores), 2) if all_diversity_scores else None,
        'diversity_std': round(np.std(all_diversity_scores), 2) if all_diversity_scores else None,
        'generation_length_mean': round(np.mean(all_gen_lengths), 2) if all_gen_lengths else None,
        'generation_length_std': round(np.std(all_gen_lengths), 2) if all_gen_lengths else None,
    }

    final_output = {
        'per_component_results': per_component_results,
        'aggregate_results': aggregate_results
    }

    print(json.dumps(aggregate_results, indent=4))

    with open(evaluation_save_path, 'w') as outfile:
        json.dump(final_output, outfile, indent=4)
