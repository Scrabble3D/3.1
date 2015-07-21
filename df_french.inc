( English:'French';
  Native:'Français';
  LangFile:'french.lang';
  DicFile:'francais.dic';
  FlagID:'fr';
  Letters:'A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z';
  LetterCount:'9,2,2,3,15,2,2,2,8,1,1,5,3,6,6,2,1,6,6,6,6,2,1,1,1,1';
  LetterValue:'1,3,3,2,1,4,2,4,1,8,10,1,2,1,1,3,8,1,1,1,1,4,10,10,10,10';
  NumberOfJokers:2;
  ReadingDirection:rdLeftToRight;
  ExcludedCat:'';
  RulesValid:true;
  NumberOfLetters:7;
  NumberOfRandoms:0;
  TimeControl:2;
  TimeControlEnd:true; //point 7
  TimeControlBuy:false; //point 7
  TimePerGame:'0:50:00'; //http://www.ffsc.fr/classique.php?page=organisation (point 8) -> "Le temps alloué recommandé par joueur et par partie est de 25 minutes."
  PenaltyValue:1; //this value ought to be zero, but zero is prohibited in the program
  PenaltyCount:1; //this value ought to be zero, but zero is prohibited in the program
  GameLostByTime:false; //point 7
  WordCheckMode:2;
  ChallengePenalty:5; //point 5.3 - a penalty of n points per word not being a feature in Scrabble3D, a setting of 5 points per move has been chosen arbitrarily
  ChallengeTime:20; //chosen arbitrarily
  JokerExchange:false;
  ChangeIsPass:false; //not explicitly mentioned, therefore derived indirectly from point 4.2 and 4.3; JE CHANGE ≠ JE PASSE
  CambioSecco:false;
  SubstractLetters:true; //point 7
  AddLetters:true; //point 7
  JokerPenalty:0;
  NumberOfPasses:3; //point 7
  LimitExchange:7; //point 4.3
  EndBonus:0;
  ScrabbleBonus:50)

{References:
http://www.fisf.net
http://www.fisf.net/documents/reglements-et-guides.html 
http://www.fisf.net/index.php?option=com_content&task=view&id=29&Itemid=52
http://www.fisf.net/index.php?option=com_docman&task=cat_view&gid=15&Itemid=45
- (RÈGLEMENT DU SCRABBLE CLASSIQUE DE COMPÉTITION, v7.5a (1er septembre 2010) 
http://www.ffsc.fr
http://www.ffsc.fr/classique.php?page=reglement
http://www.ffsc.fr/classique.php?page=organisation
-
Current state of implementation in this *.inc file: 2012-08-10}
