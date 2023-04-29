import os
import sys
import argparse
from sly import Lexer

# Constants used in the Phoenix lexer
class PhoenixConstants:
    YELLOW_TEXT = '\033[93m'
    GREEN_TEXT = '\033[92m'
    TOKEN_FILE_EXTENSION = '.phxtokens'
    NORMAL_TEXT = '\033[0m'

    # PHX lexer implementation
class PHXLexer(Lexer):

   # Set of tokens
    tokens = {ASSIGN, DECREMENT, DIVIDE, EQUAL, FLOAT, GE, GT, IDENTIFIER, INCREMENT, LE, LT, MINUS, MODULO,
                 MULTIPLY, NOT_EQUAL, NUMBER, PLUS, POW, SINGLE_QUOTES, STRING} 

    # Literals
    literals = {'{', '}', ',', '?', ';', ':', '[', ']', '(', ')', '.', '!'}

    # Ignore patterns
    ignore = ' \t'
    ignore_newline = r'\n+'
    ignore_comment = r'\#(.*)'

    # Token patterns
    EQUAL = r'=='
    ASSIGN = r'='
    DECREMENT = r'\--'
    DIVIDE = r'/'
    FLOAT = r'\d+\.\d+'
    GE = r'>='
    GT = r'>'
    IDENTIFIER = r'[a-zA-Z_][a-zA-Z0-9_]*'
    INCREMENT = r'\++'
    LE = r'<='
    LT = r'<'
    MINUS = r'-'
    MODULO = r'%'
    MULTIPLY = r'\*'
    NOT_EQUAL = r'!='
    NUMBER = r'\d+'
    PLUS = r'\+'
    POW = r'\^'
    SINGLE_QUOTES = r'\''
    STRING = r'\".*\"'

def phxReadInputFile(phxFileName):
    phxData = None
    try:
        with open(phxFileName, "r") as phxInputFile:
            phxData = phxInputFile.read()
    except FileNotFoundError:
        print("File not present in:", sys.argv[1])
    print("Source Code Scanning: " + PhoenixConstants.GREEN_TEXT + 'SUCCESS' + PhoenixConstants.NORMAL_TEXT)
    return phxData

def phxWriteTokensToFile(phxTokens, phxFileName):
    with open(phxFileName, "w") as phxFile:
        for phxToken in phxTokens:
            phxFile.write('{}\n'.format(phxToken.value))
        print("Tokens write operation " + phxFileName + ": " + PhoenixConstants.GREEN_TEXT +
              'SUCCESS' + PhoenixConstants.NORMAL_TEXT)
              
def phxParseArguments():
    phxParser = argparse.ArgumentParser(
        description='PHOENIX Lexer - Converting PHOENIX source code into a list of TOKENS and saving '
                    '<InputFileName>' + PhoenixConstants.TOKEN_FILE_EXTENSION)
    phxParser.add_argument('input', metavar='InputFileName', type=str,
                           nargs=1, help='PHOENIX source code filepath')
    phxParser.add_argument('--evaluate', action='store_true', help='Generated Token Evaluation')
    return phxParser.parse_args()

if __name__ == '__main__':
    print(PhoenixConstants.YELLOW_TEXT + "Lexer Running" + PhoenixConstants.NORMAL_TEXT)
    phxParsedArgs = phxParseArguments()
    phxInputFileName = phxParsedArgs.input[0]
    phxOutputFileName = phxParsedArgs.input[0][:-4:] + PhoenixConstants.TOKEN_FILE_EXTENSION
    phxFileData = phxReadInputFile(phxInputFileName)

    phxLexer = PHXLexer()
    phxTokens = phxLexer.tokenize(phxFileData)
    phxWriteTokensToFile(phxTokens, phxOutputFileName)

    phxEvaluateCondition = phxParsedArgs.evaluate
    if phxEvaluateCondition:
        os.system("swipl -g \"main('" + phxOutputFileName + "')\" main.pl")
