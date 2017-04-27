import random
from statistics import mode
import nltk
from nltk.corpus.reader.plaintext import PlaintextCorpusReader
from nltk.classify.scikitlearn import SklearnClassifier
from nltk.classify import ClassifierI
from sklearn.naive_bayes import MultinomialNB, BernoulliNB
from sklearn.linear_model import LogisticRegression, SGDClassifier
from sklearn.svm import SVC, LinearSVC, NuSVC



def find_features(opinion, word_features):
    """ Extracts the features for a certain opinion. """
    words = set(opinion)
    features = {}
    for word in word_features:
        features[word] = (word in words)
    return features

def train_sk_model(name, classifier, training_set):
    """ Trains one of the Scikit learning algorithms. """
    sk_classifier = SklearnClassifier(classifier)
    sk_classifier.train(training_set)
    return (name, sk_classifier)

class EnsembleClassifer(ClassifierI):
    """ A text classifer based on ensemble classification """
    def __init__(self, training_set, testing_set):
        self.ensemble = [('basic', nltk.NaiveBayesClassifier.train(training_set)),
                         train_sk_model('mnb', MultinomialNB(), training_set),
                         train_sk_model('bnb', BernoulliNB(), training_set),
                         train_sk_model('lreg', LogisticRegression(), training_set),
                         train_sk_model('sgdc', SGDClassifier(), training_set),
                         train_sk_model('lin_svc', LinearSVC(), training_set),
                         train_sk_model('nu_svc', NuSVC(), training_set)]
        self.testing_set = testing_set

    def accuracy(self):
        """ Displayes the accuracies of the various
            algorithms used in the ensemble for the
            training set
        """
        for (name, algo) in self.ensemble:
            print(name + ' accuracy percent:',
                  (nltk.classify.accuracy(algo, self.testing_set))*100)

    def classify(self, features):
        """ Classifies a feature set.
            returns a tuple of the classifcation
            and the confidence.
        """
        votes = []
        for (_, algo) in self.ensemble:
            votes.append(algo.classify(features))
        confidence = votes.count(mode(votes)) / len(votes)
        return (confidence, mode(votes))

    def show_most_useful_features(self, num=15):
        self.ensemble[0][1].show_most_informative_features(num)
   
def construct_models():
    """ Builds the classification models. """
    sources = ['Conservative', # Scalia + Rehnquist
               'Progressive'] # Ginsburg + Stevens
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

    training_subset = int(len(featuresets) * 0.9)
    training_set = featuresets[:training_subset]
    testing_set = featuresets[training_subset:]

    ensemble = EnsembleClassifer(training_set, testing_set)
    ensemble.show_most_useful_features()
    ensemble.accuracy()
    print(ensemble.classify(testing_set[0][0]))


if __name__ == 'main':
    construct_models()
