( English:'Persian';
  Native:'فارسی';
  LangFile:'';
  DicFile:'persian.dic';
  FlagID:'ir';
  Letters:'آ,ا,ب,پ,ت,ث,ج,چ,ح,خ,د,ذ,ر,ز,ژ,س,ش,ص,ض,ط,ظ,ع,غ,ف,ق,ک,گ,ل,م,ن,و,ه,ی';
  LetterCount:'1,9,3,2,6,1,2,1,2,2,4,1,7,2,1,3,4,1,1,1,1,2,1,2,2,3,2,4,7,6,4,4,8';
  LetterValue:'6,1,1,4,1,10,3,6,4,3,1,8,1,2,8,1,1,6,8,6,10,4,6,2,3,1,4,1,1,1,1,1,1';
  NumberOfJokers:2;
  ReadingDirection:rdRightToLeft;
  ExcludedCat:'1'; //page 29, VI.A.
  RulesValid:true;
  NumberOfLetters:7;
  NumberOfRandoms:0;
  TimeControl:2;
  TimeControlEnd:false;
  TimeControlBuy:true; //page 26, V.F.3.
  TimePerGame:'0:50:00'; //page 26, V.F.2.
  PenaltyValue:10; //page 26, V.F.2.
  PenaltyCount:10; //page 26, V.F.3.
  GameLostByTime:true; //page 26, V.F.3.
  WordCheckMode:2;
  ChallengePenalty:0;  //doubtful, but in fact in the rules nothing is written about that issue
  ChallengeTime:20; //page 20, IV.I.1.
  JokerExchange:false;
  ChangeIsPass:true; //page 25, V.C.2   
  CambioSecco:false;   
  SubstractLetters:true; //page 25, V.F.1. (adapted to the current features of Scrabble3D)
  AddLetters:true; //page 25, V.F.1. (adapted to the current features of Scrabble3D)
  JokerPenalty:0;
  NumberOfPasses:3; //page 25, V.C.2
  LimitExchange:7; //page 16, IV.E.2.a.
  EndBonus:0;
  ScrabbleBonus:50)

  {Comment: 
  It is understood that there are no official tournament rules for Scrabble in Persian language, 
  because there is no Scrabble Federation in Iran or any other country where Farsi is spoken. 
  There is not even a Persian edition of the board game. That's why American NASPA tournament 
  rules have arbitrarily been chosen for Scrabble games in Persian language.
  The following vague reason has been taken into consideration: Quite a few Persian people are 
  living in the USA and might know the American NASPA rules.
  -
  References:
  http://www.scrabbleplayers.org/wiki/images/9/95/Rules-20120616.pdf (NASPA, June 27, 2012) 
  http://www.scrabbleplayers.org/w/Welcome_to_NASPAWiki
  http://www.scrabbleplayers.org/w/Rules
  http://17085.homepagemodules.de/t1833f360-Standardeinstellungen-in-Scrabble-D-fuer-die-persischen-Spielregeln.html
  -
  Current state of implementation in this *.inc file: 2012-08-10 }
  
  
  
