import itertools as I
import re


def not_cases_start(line):
    return not line == 'CASES ADJUDGED\n'


def drop_until_cases(file):
    cases = I.dropwhile(not_cases_start, file)
    cases = I.islice(cases, 1, None)
    return I.dropwhile(not_cases_start, cases)


def is_case_title(line):
    return bool(re.match(r'(\w+|\s)+v\.(\w+|\s)+', line))


def is_not_case_title(line):
    return not is_case_title(line)


def is_end_of_case(line):
    return bool(re.match(r'It is so ordered.', line))


def is_not_end_of_case(line):
    return not is_end_of_case(line)


def grab_next_case(file):
    file = I.dropwhile(is_not_case_title, file)
    return I.takewhile(is_not_end_of_case, file)



def clean_data():
    filepath = 'data/502.txt'
    print('Cleaning ' + filepath)
    with open(filepath) as file:
        # Drop lines until cases, 'CASES ADJUDGED' appears twice
        file = drop_until_cases(file)

        # Strip empty lines and extra new lines
        file = filter(lambda x: not re.match(r'^\s*$', x), file)
        file = map(lambda x: x.rstrip('\n'), file)

        case1 = grab_next_case(file)
        case2 = grab_next_case(file)
        
        for line in I.islice(case1, 0, 5):
            print(line)
            
        for line in I.islice(case2, 0, 5):
            print(line)
