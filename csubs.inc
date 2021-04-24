'--> CSUBS. Two functions I couldn't get fast enough in MMBasic, used when inserting/deleting lines.
'void moveBlockDown(long long *from, long long *to, long long *numElemsp) {
' long long *endp = from + *numElemsp;
'
' while (from < endp) {
'   *to = *from;
'   ++to;
'   ++from;
' }
'}
CSUB moveBlockDown INTEGER, INTEGER, INTEGER
  00000000
  6814B410 04C4EB00 D20542A0 2302E8F0 E8E14284 D8F92302 4B04F85D BF004770 
End CSUB

'void moveBlockUp(long long *from, long long *to, long long *numElemsp) {
' long long numElems = *numElemsp;
' long long *fptr = from + numElems - 1;
' long long *tptr = to + numElems - 1;
'
' while (fptr >= from) {
'   *tptr = *fptr;
'   --tptr;
'   --fptr;
' }
'}
CSUB moveBlockUp INTEGER, INTEGER, INTEGER
  00000000
  B4306813 4460F06F EB00441C EB0103C4 429801C4 E873D805 42984502 4502E861 
  BC30D9F9 BF004770 
End CSUB

'long long getNext(long long* parserCSUBCtxt, unsigned char *lineToParse, unsigned char *cmdList) {
'  long lineToParseLen = *lineToParse;
'  long parsePos = parserCSUBCtxt[PARSE_POS];
'  long parseState = parserCSUBCtxt[PARSE_STATE];
'  long numCmds = parserCSUBCtxt[PARSER_NUM_COMMANDS];
'  long startCol = parserCSUBCtxt[PARSER_START_COL];
'  long endCol = parserCSUBCtxt[PARSER_END_COL]; //endCol is one based.
'  unsigned char *parseWhiteSpaces = cmdList; //First cmd is list of whitespaces.
'  unsigned char *seek = lineToParse + parsePos; //Seek is one based (rel. to lineToParse), as far as pointers can be one based.
'  unsigned char *frag, *fragEndp;
'  unsigned char c;
'  long ii, jj, fragLen, cmdLen;
'
'  parserCSUBCtxt[PARSER_FRAG_START] = MAX(parsePos, startCol);
'
'  if (parsePos > lineToParseLen)
'    return 0;
'
'  if (parseState == PARSE_STATE_COMMENT) { 
'    parserCSUBCtxt[PARSE_POS] = endCol;
'    return parserCSUBCtxt[PARSE_COLOR_COMMENT];
'  }
'
' if (parseState == PARSE_STATE_STRING) {
'    ii = parsePos+1;
'    while (ii < endCol) {
'      /*Note that we want to increment ii here also when there's a match.*/
'      if (lineToParse[ii++] == '\"')
'        break;
'    }
'
'    if (ii < endCol) { //if not at end, figure out the next state
'      c = lineToParse[ii];
'
'      if (c=='\'') {
'        parserCSUBCtxt[PARSE_STATE] = PARSE_STATE_COMMENT;
'      }
'      else if (c == '\"') {
'        parserCSUBCtxt[PARSE_STATE] = PARSE_STATE_STRING;
'      }
'      else {
'        jj=1;
'        while (jj <= NUM_WHITESPACES) {
'          if (c == parseWhiteSpaces[jj])
'            break;
'          ++jj;
'        }
'
'        parserCSUBCtxt[PARSE_STATE] = (jj > NUM_WHITESPACES) ? PARSE_STATE_WORD : PARSE_STATE_WHITESPACE;
'      }
'    }
'
'    parserCSUBCtxt[PARSE_POS] = ii;
'    return parserCSUBCtxt[PARSE_COLOR_STRING];
'  }

'  //Whitespace or word state.
'  while (seek <= lineToParse + lineToParseLen) { //Determine next state
'    c = *seek;
'
'    if (c == '\'') {
'      parserCSUBCtxt[PARSE_STATE] = PARSE_STATE_COMMENT;
'      break;
'    }
'
'    if (c == '\"') {
'      parserCSUBCtxt[PARSE_STATE] = PARSE_STATE_STRING;
'      break;
'    }
'
'    jj=1;
'    while (jj <= NUM_WHITESPACES) {
'      if (c == parseWhiteSpaces[jj]) break;
'      ++jj;
'    }
'
'    if ((jj <= NUM_WHITESPACES) && (parseState == PARSE_STATE_WORD)) {
'      parserCSUBCtxt[PARSE_STATE] = PARSE_STATE_WHITESPACE;
'      break;
'    }
'    
'    if ((jj > NUM_WHITESPACES) && (parseState == PARSE_STATE_WHITESPACE)) {
'      parserCSUBCtxt[PARSE_STATE] = PARSE_STATE_WORD;
'      break;
'    }
'
'    ++seek; 
'  }
'
'  fragEndp = seek;
'  parserCSUBCtxt[PARSE_POS] = MIN(seek - lineToParse, endCol);
'
'  if (parseState != PARSE_STATE_WHITESPACE) {
'    ii=1; //The 0th cmd is the whitespace list, so we start at 1.
'    while (ii < numCmds) {
'      frag = lineToParse + parsePos;
'      fragLen = fragEndp - frag;
'      seek = cmdList + CMD_LEN*(ii++);
'      cmdLen = *(seek++);
'
'      if (fragLen == cmdLen) {
'        jj=0;
'        while (jj < fragLen) {
'          c = frag[jj];
'          if (c > 0x60) c -= 0x20;
'          if (c != seek[jj]) break;
'          ++jj;
'        }
'
'        if (jj >= fragLen) {
'          return parserCSUBCtxt[PARSE_COLOR_KEYWORD];
'        }
'      }
'    }
'  }
'
'  return parserCSUBCtxt[PARSE_COLOR_FG];
'}
'
'void main(long long* parserCSUBCtxt, unsigned char *lineToParse, unsigned char *cmdList) {
'  int fragColor;
'  long long fragEnd, fragStart;
'  int fragLen;
'  long long startCol = parserCSUBCtxt[PARSER_START_COL];
'  long long endCol = parserCSUBCtxt[PARSER_END_COL];
'  int x = (int)parserCSUBCtxt[PARSER_LINE_X];
'  int y = (int)parserCSUBCtxt[PARSER_LINE_Y];
'  int x2;
'  char c = lineToParse[1];
'
'  parserCSUBCtxt[PARSE_POS] = 1; /*PARSE_POS is one based string offset.*/
'
'  if (c=='\'') {
'    parserCSUBCtxt[PARSE_STATE] = PARSE_STATE_COMMENT;
'  }
'  else if (c=='\"') {
'    parserCSUBCtxt[PARSE_STATE] = PARSE_STATE_STRING;
'  }
'  else {
'    int jj=1;
'    while (jj <= NUM_WHITESPACES) {
'      if (c == cmdList[jj])
'        break;
'      ++jj;
'    }
'
'    parserCSUBCtxt[PARSE_STATE] = (jj > NUM_WHITESPACES) ? PARSE_STATE_WORD : PARSE_STATE_WHITESPACE;
'  }
'
'  do {
'    fragColor = (int)getNext(parserCSUBCtxt, lineToParse, cmdList);
'        fragEnd = parserCSUBCtxt[PARSE_POS]; /*One based*/
'
'        if (fragEnd > startCol) {
'          fragStart = parserCSUBCtxt[PARSER_FRAG_START]; /*One based*/
'          fragLen = (int)(fragEnd - fragStart);
'          x2 = x+fragLen*COL_WIDTH;
'          DrawRectangle(x, y, x2-1, y+ROW_HEIGHT-1, fragColor);
'          x = x2;
'        }
'    } while (fragEnd < endCol); 
'
'    parserCSUBCtxt[PARSER_LINE_X] = x;
'}
CSUB syntaxHighLight INTEGER, STRING, STRING
  00000074
  'getNext
  4FF0E92D 68046C06 A000F891 688542A6 E030F8D0 4626BFB8 F8D045A2 EA4FC048 
  E9C077E6 DB72670E F0002D03 2D04808A 448AD073 0804EB01 F0C045C2 46C380C3 
  9000F89B F10B465B F1B90B01 F0000F27 F1B98088 D07E0F22 24014616 7F01F816 
  454F3401 2C11D017 2D01D1F8 45DAD07F D2E5465B 45641A5C 4664BFA8 46262D01 
  77E4EA4F 6700E9C0 E9D0D113 46204504 E8BD4629 2D028FF0 1A5CD1E9 27002601 
  BFA845A4 466446A4 E9C017E5 E9C06702 F1BE4500 DDE80F01 1E0EEB0E 0511F102 
  0308EBA3 0901F1C8 E0024472 42953511 7829D0DB D1F94299 F108B1B3 462E34FF 
  0E03EB04 45A6E001 EB09D00E F8140C04 F1A11F01 29600720 B2F9BF88 7F01F816 
  D0F0428F DBE1459C 4506E9D0 2400E001 46202500 E8BD4629 34018FF0 E004190B 
  5B01F813 2D223401 4564D02E 4622DBF8 E9D017E3 E9C04508 46292300 E8BD4620 
  46628FF0 450AE9D0 462917D3 2300E9C0 E8BD4620 26048FF0 1A5C2700 6702E9C0 
  2603E789 1A5C2700 6702E9C0 1A5AE783 25002402 BFA84594 46624694 E9C017D3 
  E9C04502 E7802300 DDD045A4 29275D09 2922D00E F102D011 F8120510 428B3F01 
  4295D012 2202D1F9 E9C02300 E7BE2302 23002203 2302E9C0 2204E7B9 E9C02300 
  E7B42302 E7564643 23002201 BF00E7ED 
  'main
  4FF0E92D 46914688 27002601 6D054604 A058F8D0 8B02ED2D 2310E9D0 7849B089 
  2302E9CD E9D02927 E9C02312 E9CD6700 D04A2304 D0432922 F109464B F8130010 
  428A2F01 4298D046 2202D1F9 E9C42300 F10A2302 EE080B0B EE088A10 464ABA90 
  1A10EE18 F7FF4620 EE18FEE3 46063A90 46284651 7800E9D4 BC02E9DD EB7C45BB 
  DA0E0208 92076BA2 F8D24A14 9A07C000 1ABA9600 6000F8DC 02C2EB05 3A014615 
  E9DD47B0 42972304 0303EB78 462ADBD7 E9C417EB B0092314 8B02ECBD 8FF0E8BD 
  23002204 2302E9C0 2203E7C3 E9C02300 E7BE2302 23002201 BF00E7B9 080002D8 
End CSUB
'<-- CSUBS

