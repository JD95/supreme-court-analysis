import nltk
from nltk.corpus.reader.plaintext import PlaintextCorpusReader
import random

def find_features(opinion, word_features):
    words = set(opinion)
    features = {}
    for w in word_features:
        features[w] = (w in words)
    return features


def main():
    sources = ['Scalia', 'Ginsburg']
    corpus = [(PlaintextCorpusReader('data/' + path + '/', '.*'), path)
              for path in sources]
    documents = []
    for (c, cat) in corpus:
        for fileid in c.fileids():
            documents.append((c.words(fileid), cat))

    random.shuffle(documents)

    all_words = []

    for (c, cat) in corpus:
        all_words.extend(c.words())

    all_words = nltk.FreqDist(all_words)

    word_features = list(all_words.keys())[:3000]

    featuresets = [(find_features(opinion, word_features), cat)for (opinion, cat) in documents]

    training_set = featuresets[:1900]
    testing_set = featuresets[1900:]
    classifier = nltk.NaiveBayesClassifier.train(training_set)

    print(len(testing_set))
    print("Classifier accuracy percent:",
          (nltk.classify.accuracy(classifier, testing_set))*100)
    
    classifier.show_most_informative_features(15)
        

if __name__ == 'main':
    main()
