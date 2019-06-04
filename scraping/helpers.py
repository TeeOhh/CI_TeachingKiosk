import string
import os


def real_dirname():
    return os.path.dirname(os.path.realpath(__file__))


def xpath_to_list(source, xpath_text):
    xpath_list = source.xpath(xpath_text)
    return xpath_list if len(xpath_list) > 0 else None


def xpath_to_clean_string(source, xpath_text):
    xpath_list = xpath_to_list(source, xpath_text)
    if xpath_list:
        utf8_safe = str(' '.join(xpath_list))
        return utf8_safe.translate(str.maketrans('', '', '\t\n')).strip()
    else:
        return None


def xpath_to_exists_bool(source, xpath_text):
    return False if xpath_to_list(source, xpath_text) is None else True


def unique_id(name):
    printable = set(string.printable)
    filtered_name = ''.join([x for x in name if x in printable])
    no_punc = filtered_name.translate(str.maketrans('', '', string.punctuation))
    return no_punc.replace(' ', '')


def lisp_style(pred, args):
    return '(' + pred + ' ' + ' '.join(args) + ')'


def isa(arg1, arg2):
    return lisp_style('isa', [arg1, arg2])


def to_file(output_data, filepath, filename):
    with open('{}/{}.krf'.format(filepath, filename), 'w+') as f:
        f.write(output_data)
