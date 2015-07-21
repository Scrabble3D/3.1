( English:'Greek';
  Native:'Ελληνικά';
  LangFile:'greek.lang';
  DicFile:'greek.dic';
  FlagID:'gr';
  Letters:'Α,Β,Γ,Δ,Ε,Ζ,Η,Θ,Ι,Κ,Λ,Μ,Ν,Ξ,Ο,Π,Ρ,Σ,Τ,Υ,Φ,Χ,Ψ,Ω';
  LetterCount:'12,1,2,2,8,1,7,1,8,4,3,3,6,1,9,4,5,7,8,4,1,1,1,3';
  LetterValue:'1,8,4,4,1,10,1,10,1,2,3,3,1,10,1,2,2,1,1,2,8,8,10,3';
  NumberOfJokers:2;
  ReadingDirection:rdLeftToRight;
  ExcludedCat:'';
  RulesValid:true;
  NumberOfLetters:7;
  NumberOfRandoms:0;
  TimeControl:2;
  TimeControlEnd:false;
  TimeControlBuy:true; //OK
  TimePerGame:'1:00:00'; //OK - Δ-χρόνος
  PenaltyValue:10; //chosen arbitrarily, because the Greek rules are too difficult to realize in Scrabble3D, see Δ-χρόνος
  PenaltyCount:10; //chosen arbitrarily, because the Greek rules are too difficult to realize in Scrabble3D, see Δ-χρόνος
  GameLostByTime:true; 
  WordCheckMode:2;
  ChallengePenalty:10; //chosen arbitrarily, because the Greek rules are too difficult to realize in Scrabble3D, see Ε-ενστάσεις
  ChallengeTime:20; //chosen arbitrarily
  JokerExchange:false;
  ChangeIsPass:true; //OK - 5.ΤΕΛΟΣ του ΑΓΩΝΑ
  CambioSecco:false;
  SubstractLetters:true; //OK - 5.ΤΕΛΟΣ του ΑΓΩΝΑ
  AddLetters:true;
  JokerPenalty:0;
  NumberOfPasses:3; //OK - 5.ΤΕΛΟΣ του ΑΓΩΝΑ
  LimitExchange:7;
  EndBonus:0;
  ScrabbleBonus:50)

  
  {References:
  http://www.greekscrabble.gr/index.php?option=com_content&task=view&id=31&Itemid=102 (Κανονισμοί) 
  http://www.greekscrabble.gr/
  http://17085.homepagemodules.de/t1762f277-Modern-Greek-Scrabble-Rules-Official-Tournament-Rules.html
  -
  Comment:
  Unfortunately several complicated Greek rules cannot be implemented as Scrabble3D settings; see the discussion on
  http://17085.homepagemodules.de/t1762f277-Modern-Greek-Scrabble-Rules-Official-Tournament-Rules.html#msg9872617 
  However, when it was possible to choose Greek tournament rules as standard settings, the comment "OK" has been added.
  Otherwise the English WESPA rules from http://www.wespa.org/rules/RulesV2nov11.pdf have been chosen instead.
  -
  Current state of implementation in this *.inc file: 2012-08-10}
  
