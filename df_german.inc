( English:'German';
  Native:'Deutsch';
  LangFile:'german.lang';
  DicFile:'deutsch.dic';
  FlagID:'de';
  Letters:'A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,Ä,Ö,Ü';
  LetterCount:'5,2,2,4,15,2,3,4,6,1,2,3,4,9,3,1,1,6,7,6,6,1,1,1,1,1,1,1,1';
  LetterValue:'1,3,4,1,1,4,2,2,1,6,4,2,3,1,2,4,10,1,1,1,1,6,3,8,10,3,6,8,6';
  NumberOfJokers:2;
  ReadingDirection:rdLeftToRight;
  ExcludedCat:'1,2';
  RulesValid:true;
  NumberOfLetters:7;
  NumberOfRandoms:0;
  TimeControl:2;
  TimeControlEnd:false;
  TimeControlBuy:true; //page 12, point 5.2
  TimePerGame:'1:00:00'; //one of the possibilities according to page 5, 1.1.b, common practice in big tournaments
  PenaltyValue:10; //page 12, point 5.2
  PenaltyCount:10; //page 12, point 5.2
  GameLostByTime:true; //page 12, point 5.2 and 5.3
  WordCheckMode:2;
  ChallengePenalty:10; //page 8, point 3.1.4 (penalty per unsuccessfully challenged word is not possible in Scrabble3D, therefore penalty per move)
  ChallengeTime:20; //page 8, point 3.1.3
  JokerExchange:false;
  ChangeIsPass:false; //not explicitly mentioned, but derived indirectly from point 5.1.b as well as point 3.2 and 3.3
  CambioSecco:false;
  SubstractLetters:true; //http://www.scrabble.de/rules.php?f=&sub=2 - point 4 i)
  AddLetters:true; //http://www.scrabble.de/rules.php?f=&sub=2 - point 4 i)
  JokerPenalty:0;
  NumberOfPasses:2; //page 12, point 5.1
  LimitExchange:7; //page 9, point 3.2.1
  EndBonus:0;
  ScrabbleBonus:50)
  
{References:
http://www.scrabble-deutschland.de/docs/SDeV_TSO.pdf (version 1.0 from 2010-04-25) 
http://www.scrabble.de/rules.php?f=&sub=2
-
Current state of implementation in this *.inc file: 2012-08-10}
