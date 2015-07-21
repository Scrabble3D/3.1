type
  TLangSetting=record
    sName    : string;
    sLetSet  : string;
    sDic     : string;
    sLang    : string;
    sFlag    : string;
    aExcl    : TByteSet;
    nLet     : byte;    //fmGameOptions.seNumberOfLetters.Value:=7;
    nRand    : byte;    //fmGameOptions.seNumberOfRandomLetters.Value:=0;
    aTCtrl   : byte;    //fmGameOptions.rgTimeControl.ItemIndex:=0;
    aTEnd    : boolean; //fmGameOptions.rbTimePenaltyEndGame.Checked:=true;
    aTBuy    : boolean; //fmGameOptions.rbTimePenaltyBuyTime.Checked:=true;
    sTGame   : string;  //fmGameOptions.meTimeLimitPerGame.Text:='0:25:00';
    vPenVal  : byte;    //fmGameOptions.seTimePenaltyValue.Value:=10;
    nPenCnt  : byte;    //fmGameOptions.seTimePenaltyCount.Value:=10;
    aWrdChk  : byte;    //fmGameOptions.rgWordCheckMode.ItemIndex:=1;//Poll (0=Takeback, 2=Challenge)
    vChalPen : byte;    //fmGameOptions.seChallengePenalty.Value:=10;//Challenge value
    vChalTim : byte;    //fmGameOptions.seChallengePeriod.Value:=20; //Challenge value
    aJkEx    : boolean; //fmGameOptions.cbCanJokerExchange.Checked:=false;
    aChgPss  : boolean; //fmGameOptions.cbChangeIsPass.Checked:=false;
    aCmb     : boolean; //fmGameOptions.cbCambioSecco.Checked:=false;
    aSub     : boolean; //fmGameOptions.cbSubstractLettersLeft.Checked:=true;
    aAdd     : boolean; //fmGameOptions.cbAddLettersLeft.Checked:=true;
    vJokPen  : byte;    //fmGameOptions.seJokerPenalty.Value:=0;
    nPas     : byte;    //fmGameOptions.seNumberOfPasses.Value:=3;
    vLimEx   : byte;    //fmGameOptions.seLimitExchange.Value:=7;
    vEndBon  : byte;    //fmGameOptions.seGameEndBonus.Value:=0;
    nMult    : byte;    //fmGameOptions.seLettersetMultiplier.Value:=1;
    aBrd     : byte;    //fmGameOptions.cbBoard.ItemIndex:=0;
//    :byte;//fmGameOptions.seNumberOfJokers.Value:=2;
  end;
const
//  nLet:7;nRand:0;aTCtrl:0;aTEnd:false;aTBuy:true;sTGame:'0:25:00';vPenVal:10;nPenCnt:10;aWrdChk:1;vChalPen:10;vChalTim:20;aJkEx:false;aChgPss:false;aCmb:false;aSub:true;aAdd:true;vJokPen:0;nPas:3;vLimEx:7;vEndBon:0;nMult:1;aBrd:0;
  DefaultLangSettings:array[0..9] of TLangSetting=
    (
     );//0,'0:25:00',
