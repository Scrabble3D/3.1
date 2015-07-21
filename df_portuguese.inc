( English:'Portuguese';
  Native:'Português europeu';
  LangFile:'portuguese.lang';
  DicFile:'portuguese.dic';
  FlagID:'pt';            //flag icon
  Letters:'A,B,C,Ç,D,E,F,G,H,I,J,L,M,N,O,P,Q,R,S,T,U,V,X,Z';
  LetterCount:'14,3,4,2,5,11,2,2,2,10,2,5,6,4,10,4,1,6,8,5,7,2,1,1';
  LetterValue:'1,3,2,3,2,1,4,4,4,1,5,2,1,3,1,2,6,1,1,1,1,4,8,8';
  NumberOfJokers:3;       //3 jokers
  ReadingDirection:rdLeftToRight; //western reading direction
  ExcludedCat:'';         //there are no categories in dic
  RulesValid:true;        //internal flag
  NumberOfLetters:7;      //number of tiles on rack
  NumberOfRandoms:0;      //number of random letters, added to the distribution
  TimeControl:2;          //time limit; 0=no limit, 1=per move, 2=per game
  TimeControlEnd:false;   //true=end immediately, false=TimeControlBuy
  TimeControlBuy:true;    //true=additional minutes after time-out, false=TimeControlEnd
  TimePerGame:'0:50:00';  //total time of game (divide by number of players)
  PenaltyValue:10;        //the penalty for 1 additional minute
  PenaltyCount:10;        //how often additional minutes can be added
  GameLostByTime:true;    //zero result when out of time
  WordCheckMode:2;        //mode of word check: 0=takeback, 1=poll, 2=challenge
  ChallengePenalty:10;    //penalty when challenge was wrong
  ChallengeTime:20;       //how long players can challenge
  JokerExchange:false;    //allow to exchange placed jokers by the corresponding tile
  ChangeIsPass:true;      //treat exchange of tiles from rack as pass or not
  CambioSecco:false;      //cambio secco
  SubstractLetters:true;  //deduct remaining tiles' value from result
  AddLetters:true;    //winner get all values that have been substracted
  JokerPenalty:0;     //penlty for left jokers after game end
  NumberOfPasses:3;   //how often every player is allowed to pass in sequence
  LimitExchange:7;    //number of tiles that have to be left in bag for exchanging tiles on rack
  EndBonus:0;         //bonus for the player who ends the game
  ScrabbleBonus:50)   //bonus when all tiles have been placed

