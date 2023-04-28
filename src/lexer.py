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


    # Ignore patterns
    phxignore = ' \t'
    phxignore_newline = r'\n+'
    phxignore_comment = r'\#(.*)'

    # Literals
    phxliterals = {'{', '}', ',', '?', ';', ':', '[', ']', '(', ')', '.', '!'}

    # Set of tokens
    phxtokens = {phxASSIGN, phxDECREMENT, phxDIVIDE, phxEQUAL, phxFLOAT, phxGE, phxGT, phxIDENTIFIER, phxINCREMENT, phxLE, phxLT, phxMINUS, phxMODULO,
                 phxMULTIPLY, phxNOT_EQUAL, phxNUMBER, phxPLUS, phxPOW, phxSINGLE_QUOTES, phxSTRING}

    # Token patterns
    phxASSIGN = r'='
    phxDECREMENT = r'\--'
    phxDIVIDE = r'/'
    phxEQUAL = r'=='
    phxFLOAT = r'\d+\.\d+'
    phxGE = r'>='
    phxGT = r'>'
    phxIDENTIFIER = r'[a-zA-Z_][a-zA-Z0-9_]*'
    phxINCREMENT = r'\++'
    phxLE = r'<='
    phxLT = r'<'
    phxMINUS = r'-'
    phxMODULO = r'%'
    phxMULTIPLY = r'\*'
    phxNOT_EQUAL = r'!='
    phxNUMBER = r'\d+'
    phxPLUS = r'\+'
    phxPOW = r'\^'
    phxSINGLE_QUOTES = r'\''
    phxSTRING = r'\".*\"'
