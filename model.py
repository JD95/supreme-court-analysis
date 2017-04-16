import nltk
from os import listdir
from os.path import isfile, join
from nltk.tokenize import sent_tokenize, word_tokenize
from itertools import chain
from functools import reduce
import pickle

def unwanted_words(word):
    wanted = ['JJ', 'JJR', 'JJS', 'NN', 'NNS', 'NNP', 'NNPS',
              'RB', 'RBR', 'RBS', 'VB', 'VBD', 'VBG', 'VBN',
              'VBP', 'VBZ']
    return word[1] in wanted


def process_opinion(text):
    return filter(unwanted_words, nltk.pos_tag(word_tokenize(text)))


def fst(tup):
    return tup[0]


def collect_corpus(path, output_path):
    data_set = []

    for data in [join(path, f) for f in listdir(path) if isfile(join(path, f))]:
        with open(data) as file:
            text = file.read().replace('\n', '')
            data_set.extend(list(map(fst, process_opinion(text))))

    with open('data/' + output_path, 'w+') as output:
        for word in data_set:
            output.write("%s\n" % word)

def setup_corpus():
    print('Collecting Scalia stuff...')
    collect_corpus('data/Scalia/', 'scalia_set.txt')

    print('Collecting Ginsburg stuff...')
    collect_corpus('data/Ginsburg/', 'ginsburg_set.txt')


def gen_category(vocab, words, name):
    cat = {}

    for word in vocab:
        cat[word] = False  #/word in words

    return (cat, name)


def load_features(names):
    data_sets = list(map(lambda s: ('data/' + s + '_set.txt', s), names))
    categories = []
    vocab = []
    
    # Generate [([word], name)]
    for path_name in data_sets:
        (path, name) = path_name
        with open(path) as data:
            words = []
            for word in data:
                words.append(word.strip())
            print('processed ' + name + ' ' + str(words[0:5]))
            categories.append((words, name))
            vocab.extend(words)
        
    return [gen_category(vocab, words, name) for (words, name) in categories]

def create_classifier():
    print('Loading Features')
    features = load_features(['ginsburg', 'scalia'])
    classifier = nltk.NaiveBayesClassifier.train(features)
    classifier.show_most_informative_features(15)
    print('done')
    save_classifier = open("model.bayes", "wb")
    pickle.dump(classifier, save_classifier)
    save_classifier.close()

def load_classifer():
    classifier_f = open("model.bayes", "rb")
    classifier = pickle.load(classifier_f)
    classifier_f.close()
    return classifier

def classify(model, text):
    {word.lower(): (word in word_tokenize(text.lower())) for word in a}
    
def main():
    model = load_classifer()
    model.classify('Hello test')

if __name__ == 'main':
    main()
