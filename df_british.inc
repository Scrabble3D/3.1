( English:'English';
  Native: 'British English';
  LangFile:'english.lang';
  DicFile:'english.dic';
  FlagID:'gb';
  Letters:'A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z';
  LetterCount:'9,2,2,4,12,2,3,2,9,1,1,4,2,6,8,2,1,6,4,6,4,2,2,1,2,1';
  LetterValue:'1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10';
  NumberOfJokers:2;
  ReadingDirection:rdLeftToRight;
  ExcludedCat:''; 
  RulesValid:true;
  NumberOfLetters:7;
  NumberOfRandoms:0;
  TimeControl:2;
  TimeControlEnd:false;
  TimeControlBuy:true;
  TimePerGame:'0:50:00'; //page 22, 5.3.5
  PenaltyValue:10; //page 21, 5.3.2
  PenaltyCount:10; //page 21, 5.3.3
  GameLostByTime:true; //page 21, 5.3.3
  WordCheckMode:2;
  ChallengePenalty:5; //one of the possibilities according to WESPA v2, see page 17, 3.10.15 and page 18, 3.10.15 (d)
  ChallengeTime:20; //chosen arbitrarily, not enshrined in the rules, see page 15, 3.10.3
  JokerExchange:false;
  ChangeIsPass:true; //page 21, 5.2
  CambioSecco:false;
  SubstractLetters:true; //page 21, 5.1.4
  AddLetters:true; //page 21, 5.1.4
  JokerPenalty:0;
  NumberOfPasses:3; //page 21, 5.2
  LimitExchange:7; //page 11, 3.2.1
  EndBonus:0;
  ScrabbleBonus:50)

{References: 
http://www.absp.org.uk/index.shtml
http://www.wespa.org/index.shtml
http://www.wespa.org/rules/RulesV2nov11.pdf (WESPA 2.0 from 2010-11-17, valid since 2012-01-01)
http://www.absp.org.uk/publications/GameRules.pdf (previous ABSP rules, not valid any more since 2012-01-01)
-
Current state of implementation in this *.inc file: 2012-08-10}
