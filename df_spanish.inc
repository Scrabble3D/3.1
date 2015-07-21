( English:'Spanish';
  Native:'Español';
  LangFile:'spanish.lang';
  DicFile:'espanol.dic';
  FlagID:'es';
  Letters:'A,B,C,D,E,F,G,H,I,J,L,M,N,Ñ,O,P,Q,R,S,T,U,V,X,Y,Z,1,2,3';
  LetterCount:'12,2,4,5,12,1,2,2,6,1,4,2,5,1,9,2,1,5,6,4,5,1,1,1,1,1,1,1';
  LetterValue:'1,3,3,2,1,4,2,4,1,8,1,3,1,8,1,3,5,1,1,1,1,4,8,4,10,5,8,8';
  NumberOfJokers:2;
  ReadingDirection:rdLeftToRight;
  ExcludedCat:'';
  RulesValid:true;
  NumberOfLetters:7;
  NumberOfRandoms:0;
  TimeControl:2;
  TimeControlEnd:false; //not explicitly mentioned in the rules, but derived indirectly from point 35 
  TimeControlBuy:true; //point 35
  TimePerGame:'0:50:00'; //point 34
  PenaltyValue:10; //point 35
  PenaltyCount:255; //point 35  (since no limit is explicitly mentioned in the rules, the value 255 has been chosen)  
  GameLostByTime:false; //nothing about that is explicitly mentioned in the rules
  WordCheckMode:2;
  ChallengePenalty:0; //no challenge penalty is mentioned in the rules
  ChallengeTime:20; //nothing about that issue is found in the rules, that's why the value has been chosen arbitrarily
  JokerExchange:false;
  ChangeIsPass:false; //point 45.2
  CambioSecco:false;
  SubstractLetters:true; //point 45.4
  AddLetters:true; //point 45.4
  JokerPenalty:0;
  NumberOfPasses:2; //point 45.2 (point 45.3 is not a feature in Scrabble3D)
  LimitExchange:1; //no limit is explicitly mentioned in the rules, therefore the value 1 has been chosen
  EndBonus:0;
  ScrabbleBonus:50)

  {References:
  http://fisescrabble.org/reglamentos/modalidad-clasica/
  http://fisescrabble.org
  http://www.ajscrabble.org/joomla/index.php/reglamento/31-reglamento-partidas-clasicas/198-reglamento-partida-clasica-fise
  -
  Current state of implementation in this *.inc file: 2012-08-10 } 
