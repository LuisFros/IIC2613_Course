### SVM sufre con un feature space muy grande, hacer codificacion ONE-HOT crea un feature space grande. 
#  ## BagOfWords se pierde el orden al sumar los valoress one_hot 
## El incide indica al suporting facts 
## 1-current questions
## si hay tab o signo de pregunta en la linea, es una pregunta o no

from nltk.tokenize import RegexpTokenizer
from sklearn.metrics import accuracy_score, confusion_matrix,classification_report
from sklearn.svm import LinearSVC
from sklearn.neural_network import MLPClassifier
from sklearn.model_selection import GridSearchCV
from time import time
import numpy as np
import os 
from nltk.corpus import stopwords
from nltk.stem import PorterStemmer 
from math import floor
from ipy_table import *
from sklearn.exceptions import ConvergenceWarning
import warnings

warnings.filterwarnings('ignore', category=ConvergenceWarning)

TOKENIZER=RegexpTokenizer(r'([a-zA-Z]+|\?)')
## todas las palabras o los signos de pregunta. El signo de pregunta le puede entregar informacion (supuesto ayudante)


def tokenize(input_,stop_words):
    ## recibe un string, y retorna una lista con los tokens que le pertenece
    tokens=TOKENIZER.tokenize(input_)
    stemmer = PorterStemmer()
    st=stemmer.stem
    lowered_tokens=[st(token.lower()) for token in tokens if token not in stop_words]
    return lowered_tokens

def parse_stories(story):
    substories=[]
    stop_words=[]
    # stop_words=set(stopwords.words('english'))
    for line in story:
        line_id,statement=line.split(" ",maxsplit=1)
        if line_id=="1":
            substory=[]
        if "\t" in statement:
            ## maneajr Q and Answer
            query,answer,_=statement.split("\t")
            query=tokenize(query,stop_words)
            ## hacemos una copia de la lista porque es mutable
            substories.append((substory[:],query,answer))
        else:
            ## manejar un fact
            statement=tokenize(statement,stop_words)
            substory.append(statement)
    return substories

def get_max_substory_length(stories):
    # for i in stories:
    #     print(i)
    #     print(type(i))

    substories_lenght=[len(substory) for substory,_,_ in stories]
    return max(substories_lenght)

PAD_TOKEN="_PAD"
def pad_substories(stories,max_lenght):
    for substory,_,_ in stories:
        for _ in  range(max_lenght-len(substory)):
            substory.append([PAD_TOKEN])
    return stories
## n tamano del vocavu

def get_vocabs(stories):
    stories_tokens=[]
    answers_tokens=[]
    for substory,query,answer in stories:
        stories_tokens +=[token for fact in substory for token in fact]
        stories_tokens +=[token for token in query]
        answers_tokens +=[answer]
    stories_vocab=sorted(set(stories_tokens))
    answers_vocab=sorted(set(answers_tokens))
    stories_token_map={token: i for i,token in enumerate(stories_vocab)}
    answers_token_map={token: i for i,token in enumerate(answers_vocab)}
    return stories_vocab, stories_token_map, answers_vocab, answers_token_map

def one_hot_vector(i,dim):
    vector=np.zeros(dim) ## vector de puros zeors *dim
    vector[i]=1
    return vector

def one_hot_encode(stories,stories_token_map,answers_token_map):
    stories_encoded=[]
    stories_vocab_size=len(stories_token_map)
    # answers_vocab_size=len(answers_token_map)
    for substory,query,answer in stories:
        statements_encoded=[]
        for statement in substory:
            tokens_encoded=[one_hot_vector(stories_token_map[token],stories_vocab_size) for token in statement]
            ## ahora la suma de bag of words
            statements_encoded.append(sum(tokens_encoded))
        question_encoded=(sum([one_hot_vector(stories_token_map[token],stories_vocab_size) for token in query]))
        statements_encoded.append(question_encoded) ## agregarle la pregunta al final de los statements
        answer_encoded=answers_token_map[answer]
        ## statmens encoded es una lista de vecotres, np. conactenate apila los vectores
        stories_encoded.append((np.concatenate(statements_encoded),answer_encoded))
    return stories_encoded



def evaluate_classifier(clf,test_stores,test_answers,target_names):
    test_predicted=clf.predict(test_stores)
    ## (respuestas reales
    accuracy=accuracy_score(test_answers,test_predicted)
    conf_matr=confusion_matrix(test_answers,test_predicted)
    print(classification_report(test_answers,test_predicted,labels=clf.classes_,target_names=target_names))
    print(conf_matr)
    print(accuracy)
    return accuracy
 
def load_qa(DATASET_PATH="IIC2613_Course\en\qa6_yes-no-questions",other=False):
    if other:
        lista_dir=os.listdir("D:/IA Github/IIC2613_Course/en/")
        train_stories=[]
        test_stories=[]
        for el in lista_dir:
            if "qa" in el and "train" in el:
                with open("IIC2613_Course\en\{}".format(el)) as file:
                    train_stories+=parse_stories(file.readlines())
            elif "qa" in el and "test" in el:
                with open("IIC2613_Course\en\{}".format(el)) as file:
                    test_stories+=parse_stories(file.readlines())
        test_stories=np.array(test_stories)
        train_stories=np.array(train_stories)
    else:     
        with open(DATASET_PATH+"_train.txt") as train_file:
            train_stories=parse_stories(train_file.readlines())
        with open(DATASET_PATH+"_test.txt") as test_file:
            test_stories=parse_stories(test_file.readlines())
    max_substory_length=get_max_substory_length(np.concatenate((train_stories,test_stories)))
    padded_train_stories=pad_substories(train_stories,max_substory_length)
    padded_test_stories=pad_substories(test_stories,max_substory_length)
    stories_vocab,stories_token_map,answers_vocab,answers_token_map=get_vocabs(padded_train_stories+padded_test_stories)
    encoded_train_stories=one_hot_encode(padded_train_stories,stories_token_map,answers_token_map)
    encoded_test_stories=one_hot_encode(padded_test_stories,stories_token_map,answers_token_map)
    stories_vocab_size=len(stories_vocab)
    answers_vocab_size=len(answers_vocab)
    feature_space_size=len(encoded_train_stories[0][0])
    return encoded_train_stories,encoded_test_stories,stories_token_map,answers_token_map,max_substory_length


def multiple_nn(train_stories,train_answers,test_stories,test_answers):
    results=[]
    # hidden_layer_mean=floor((len(train_stories)+len(train_answers))/2) ## hidden layer es mean of input/output layer
    # out_four_step=floor((hidden_layer_mean)/8)
    for number_atributes in range(1,100,10):
        clf = MLPClassifier(max_iter=100,solver='sgd',hidden_layer_sizes=(number_atributes,), random_state=2018,learning_rate="adaptive") #random state = seed
        clf.fit(train_stories,train_answers)
        
        train_predicted=clf.predict(train_stories)
        ## (respuestas reales
        accuracy_t=accuracy_score(train_answers,train_predicted)


        test_predicted=clf.predict(test_stories)
        ## (respuestas reales

        accuracy=accuracy_score(test_answers,test_predicted)
        results.append((accuracy,number_atributes,clf))

    return max(results,key=lambda x:x[0])

def multiple_svm(train_stories,train_answers,test_stories,test_answers):
    negative_exp=[-i for i in range(1,6)] ## -5 
    positive_exp=[i for i in range(6,16)]
    results=[]
    for i in range(len(negative_exp)):  
        c=2**negative_exp[i]
        svm2=LinearSVC(
            penalty="l2",
            loss="squared_hinge",
            C=c,
            max_iter=10000
        )
        svm2.fit(train_stories,train_answers)

        train_predicted=svm2.predict(train_stories)
        ## (respuestas reales
        accuracy_t=accuracy_score(train_answers,train_predicted)


        test_predicted=svm2.predict(test_stories)
        ## (respuestas reales
        accuracy=accuracy_score(test_answers,test_predicted)
        results.append((abs(accuracy-accuracy_t),negative_exp[i],svm2))

    for i in range(len(positive_exp)):  
        svm=LinearSVC(
            penalty="l2",
            loss="squared_hinge",
            C=2**positive_exp[i],
            max_iter=10000
        )
        svm.fit(train_stories,train_answers)

        train_predicted=svm.predict(train_stories)
        ## (respuestas reales
        accuracy_t=accuracy_score(train_answers,train_predicted)

        test_predicted=svm.predict(test_stories)
        ## (respuestas reales
        accuracy=accuracy_score(test_answers,test_predicted)
        results.append((abs(accuracy-accuracy_t),positive_exp[i],svm))
    return max(results,key=lambda x:x[0])

def main_svm():
    # lista_dir=os.listdir("D:/IA Github/IIC2613_Course/en/")
    lista_dir=["qa20_agents-motivations_train.txt"]
    train_stories=[]
    test_stories=[]
    statistics={}
    for el in lista_dir:
        if "qa" in el and "train" in el:
            official=el
            el="IIC2613_Course\en\{}".format(el.replace("_train.txt",""))
            train_split,test_split,stories_token_map,answers_token_map,number_atributes=load_qa(el,False)
            train_stories,train_answers=zip(*train_split)
            test_stories,test_answers=zip(*test_split)
            best_svm=multiple_svm(train_stories,train_answers,test_stories,test_answers)
            # svm=LinearSVC(
                # penalty="l2",
                # loss="squared_hinge",
                # C=1
            # )
            # svm.fit(train_stories,train_answers)
            print(best_svm)
            svm=best_svm[2]
            print("Evaluar sobre el split de training")
            train_ac=evaluate_classifier(svm,train_stories,train_answers,answers_token_map.keys())

            print("Evaluar sobre el split de testing")
            test_ac=evaluate_classifier(svm,test_stories,test_answers,answers_token_map.keys())
            qa_string=official.split("_")[0]
            number_qa=int(qa_string.split("qa")[-1])
            statistics[number_qa]={"C":svm.C,"test_accuracy":test_ac,"train_accuracy":train_ac}
    return statistics


def main_nn():
    lista_dir=os.listdir("D:/IA Github/IIC2613_Course/en/")
    # lista_dir=["qa20_agents-motivations_train.txt"]
    train_stories=[]
    test_stories=[]
    statistics={}
    for el in lista_dir:
        if "qa" in el and "train" in el:
            official=el
            qa_string=official.split("_")[0]
            number_qa=int(qa_string.split("qa")[-1])
            if number_qa!=3:
                continue
            official=el
            el="IIC2613_Course\en\{}".format(el.replace("_train.txt",""))
            # el="{}".format(el.replace("_train.txt",""))

            train_split,test_split,stories_token_map,answers_token_map,number_atributes=load_qa(el,False)
            train_stories,train_answers=zip(*train_split)
            test_stories,test_answers=zip(*test_split)
            # parameters = {'solver': ['lbfgs'], 'max_iter': [500,1000,1500], 'alpha': 10.0 ** -np.arange(1, 7), 'hidden_layer_sizes':np.arange(len(train_stories),floor((len(train_stories)+len(train_answers)/2))), 'random_state':[0,1,2,3,4,5,6,7,8,9]}
            # clf_grid=GridSearchCV(MLPClassifier(), parameters, n_jobs=-1)

            best=multiple_nn(train_stories,train_answers,test_stories,test_answers)
            print(best)
            clf=best[-1]
            # clf_grid.fit(train_stories,train_answers)
            # print(clf_grid.best_score_)
            # print(clf_grid.best_params_)

            print("Evaluar sobre el split de training")
            train_ac=evaluate_classifier(clf,train_stories,train_answers,answers_token_map.keys())

            print("Evaluar sobre el split de testing")
            test_ac=evaluate_classifier(clf,test_stories,test_answers,answers_token_map.keys())
            qa_string=official.split("_")[0]
            number_qa=int(qa_string.split("qa")[-1])
            print(test_ac,train_ac)
            statistics[number_qa]={"Size":best[1],"test_accuracy":test_ac,"train_accuracy":train_ac}
            break
    return statistics
    ## matriz de confusion debe tener las diagonales cargadas

    # best=multiple_svm(train_stories,train_answers,test_stories,test_answers)
    # evaluate_classifier(best[2],train_stories,train_answers,answers_token_map.keys())
    # evaluate_classifier(svm,test_stories,test_answers,answers_token_map.keys())

    ## preceptron de multicapa, jugar con hidden layer_ sizes.

    # clf = MLPClassifier(solver='sgd',
    # hidden_layer_sizes=(15,), random_state=1) #random state = seed
    # clf.fit(train_stories,train_answers)
    # evaluate_classifier(clf,train_stories,train_answers,answers_token_map.keys())

'''
precision: proposicion que tu predijiste correctamente
recall: proporcion que 

f1-score: promedio armonico entre las 2 metricas y comparar entre clases como le esta llenda (usarlo)

La tarea 20 es la que mejor le va a SVM en esta tarea
La distribucion de las clases no es uniforme, SVM sufre con eso. (esa info esta en "support") 
class weight es un paramentro para pasarle la proporcion de la clase para el training de SVM
        El peso es inveramente propocional al numeor de veces que aprenece, peso grande a los que aprenecn poco

'''
diction=main_nn()

# lista=[[i,diction[i]["Size"],diction[i]["train_accuracy"],diction[i]["test_accuracy"]] for i in diction]
# make_table(lista)

# diction=main_svm()
