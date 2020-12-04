#include "ARMCFunctions.h"

#define PARSE_STATE_INITIAL 0LL
#define PARSE_STATE_WHITESPACE 1LL
#define PARSE_STATE_WORD 2LL
#define PARSE_STATE_COMMENT 3LL
#define PARSE_STATE_STRING 4LL

#define PARSE_POS 0
#define PARSE_STATE 1
#define PARSE_COLOR_FG 2
#define PARSE_COLOR_KEYWORD 3
#define PARSE_COLOR_STRING 4
#define PARSE_COLOR_COMMENT 5
#define PARSER_NUM_COMMANDS 6
#define PARSER_FRAG_START 7
#define PARSER_START_COL 8
#define PARSER_END_COL 9
#define PARSER_LINE_X 10
#define PARSER_LINE_Y 11

#define NUM_WHITESPACES 16
#define CMD_LEN 17

#define MAX(a,b) (((a) > (b)) ? (a) : (b))
#define MIN(a,b) (((a) < (b)) ? (a) : (b))

#define COL_WIDTH 8
#define ROW_HEIGHT 12

long long getNext(long long* parserCSUBCtxt, unsigned char *lineToParse, unsigned char *cmdList) {
	long lineToParseLen = *lineToParse;
	long parsePos = parserCSUBCtxt[PARSE_POS];
	long parseState = parserCSUBCtxt[PARSE_STATE];
	long numCmds = parserCSUBCtxt[PARSER_NUM_COMMANDS];
	long startCol = parserCSUBCtxt[PARSER_START_COL];
	long endCol = parserCSUBCtxt[PARSER_END_COL]; //endCol is one based.
	unsigned char *parseWhiteSpaces = cmdList; //First cmd is list of whitespaces.
	unsigned char *seek = lineToParse + parsePos; //Seek is one based (rel. to lineToParse), as far as pointers can be one based.
	unsigned char *frag, *fragEndp;
	unsigned char c;
	long ii, jj, fragLen, cmdLen;

	parserCSUBCtxt[PARSER_FRAG_START] = MAX(parsePos, startCol);

	if (parsePos > lineToParseLen)
		return 0;

	if (parseState == PARSE_STATE_COMMENT) { 
		parserCSUBCtxt[PARSE_POS] = endCol;
		return parserCSUBCtxt[PARSE_COLOR_COMMENT];
	}

	if (parseState == PARSE_STATE_STRING) {
		ii = parsePos+1;
		while (ii < endCol) {
			/*Note that we want to increment ii here also when we have a match.*/
			if (lineToParse[ii++] == '\"')
				break;
		}

		if (ii < endCol) { //if not at end, figure out the next state
			c = lineToParse[ii];

			if (c=='\'') {
				parserCSUBCtxt[PARSE_STATE] = PARSE_STATE_COMMENT;
			}
			else if (c == '\"') {
				parserCSUBCtxt[PARSE_STATE] = PARSE_STATE_STRING;
			}
			else {
				jj=1;
				while (jj <= NUM_WHITESPACES) {
					if (c == parseWhiteSpaces[jj])
						break;
					++jj;
				}

				parserCSUBCtxt[PARSE_STATE] = (jj > NUM_WHITESPACES) ? PARSE_STATE_WORD : PARSE_STATE_WHITESPACE;
			}
		}

		parserCSUBCtxt[PARSE_POS] = ii;
		return parserCSUBCtxt[PARSE_COLOR_STRING];
	}

	//Whitespace or word state.
	while (seek <= lineToParse + lineToParseLen) { //Determine next state
		c = *seek;

		if (c == '\'') {
			parserCSUBCtxt[PARSE_STATE] = PARSE_STATE_COMMENT;
			break;
		}

		if (c == '\"') {
			parserCSUBCtxt[PARSE_STATE] = PARSE_STATE_STRING;
			break;
		}

		jj=1;
		while (jj <= NUM_WHITESPACES) {
			if (c == parseWhiteSpaces[jj]) break;
			++jj;
		}

		if ((jj <= NUM_WHITESPACES) && (parseState == PARSE_STATE_WORD)) {
			parserCSUBCtxt[PARSE_STATE] = PARSE_STATE_WHITESPACE;
			break;
		}
		
		if ((jj > NUM_WHITESPACES) && (parseState == PARSE_STATE_WHITESPACE)) {
			parserCSUBCtxt[PARSE_STATE] = PARSE_STATE_WORD;
			break;
		}

		++seek; 
	}

	fragEndp = seek;
	parserCSUBCtxt[PARSE_POS] = MIN(seek - lineToParse, endCol);

	if (parseState != PARSE_STATE_WHITESPACE) {
		ii=1; //The 0th cmd is the whitespace list, so we start at 1.
		while (ii < numCmds) {
			frag = lineToParse + parsePos;
			fragLen = fragEndp - frag;
			seek = cmdList + CMD_LEN*(ii++);
			cmdLen = *(seek++);

			if (fragLen == cmdLen) {
				jj=0;
				while (jj < fragLen) {
					c = frag[jj];
					if (c > 0x60) c -= 0x20;
					if (c != seek[jj]) break;
					++jj;
				}

				if (jj >= fragLen) {
					return parserCSUBCtxt[PARSE_COLOR_KEYWORD];
				}
			}
		}
	}

	return parserCSUBCtxt[PARSE_COLOR_FG];
}

void main(long long* parserCSUBCtxt, unsigned char *lineToParse, unsigned char *cmdList) {
	int fragColor;
	long long fragEnd, fragStart;
	int fragLen;
	long long startCol = parserCSUBCtxt[PARSER_START_COL];
	long long endCol = parserCSUBCtxt[PARSER_END_COL];
	int x = (int)parserCSUBCtxt[PARSER_LINE_X];
	int y = (int)parserCSUBCtxt[PARSER_LINE_Y];
	int x2;
	char c = lineToParse[1];

	parserCSUBCtxt[PARSE_POS] = 1; /*PARSE_POS is one based string offset.*/

	if (c=='\'') {
		parserCSUBCtxt[PARSE_STATE] = PARSE_STATE_COMMENT;
	}
	else if (c=='\"') {
		parserCSUBCtxt[PARSE_STATE] = PARSE_STATE_STRING;
	}
	else {
		int jj=1;
		while (jj <= NUM_WHITESPACES) {
			if (c == cmdList[jj])
				break;
			++jj;
		}

		parserCSUBCtxt[PARSE_STATE] = (jj > NUM_WHITESPACES) ? PARSE_STATE_WORD : PARSE_STATE_WHITESPACE;
	}

	do {
		fragColor = (int)getNext(parserCSUBCtxt, lineToParse, cmdList);
        fragEnd = parserCSUBCtxt[PARSE_POS]; /*One based*/

        if (fragEnd > startCol) {
          fragStart = parserCSUBCtxt[PARSER_FRAG_START]; /*One based*/
          fragLen = (int)(fragEnd - fragStart);
          x2 = x+fragLen*COL_WIDTH;
          DrawRectangle(x, y, x2-1, y+ROW_HEIGHT-1, fragColor);
          x = x2;
        }
  	} while (fragEnd < endCol);	

  	parserCSUBCtxt[PARSER_LINE_X] = x;
}
