{ Routines for default settings

  v3.1.3; 2015-Mar-01
  Copyleft (c) GPLv3: 1996-2015 Heiko Tietze heiko_tietze@web.de

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit udefaults;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LResources;

type

  TDefaultsLanguage=(dfBrazilian,dfCatalan,dfDutch,dfAmericanEnglish,dfBritishEnglish,
                     dfFinnish,dfFrench,dfGerman,dfGreek,dfHebrew,dfIrish,
                     dfItalian,dfLatin,dfPersian,dfPortuguese,dfRomanian,dfRussian,
                     dfScottish,dfSlovak,dfSpanish,dfSwedish,dfTurkish);
  TReadingDirection=(rdLeftToRight,rdRightToLeft);
  TDefaultType=(dt_Dictionary,dt_Language,dt_LetterSet,dt_Rules,dt_Board);

  TDefaultValues=record
    English          : string;
    Native           : string;
    LangFile         : string;
    DicFile          : string;
    FlagID           : string;
    Letters          : string;
    LetterCount      : string;
    LetterValue      : string;
    NumberOfJokers   : byte;
    ReadingDirection : TReadingDirection;
    ExcludedCat      : string;
    RulesValid       : boolean;
    NumberOfLetters  : byte;    //fmGameOptions.seNumberOfLetters.Value:=7;
    NumberOfRandoms  : byte;    //fmGameOptions.seNumberOfRandomLetters.Value:=0;
    TimeControl      : byte;    //fmGameOptions.rgTimeControl.ItemIndex:=0;
    TimeControlEnd   : boolean; //fmGameOptions.rbTimePenaltyEndGame.Checked:=true;
    TimeControlBuy   : boolean; //fmGameOptions.rbTimePenaltyBuyTime.Checked:=true;
    TimePerGame      : string;  //fmGameOptions.meTimeLimitPerGame.Text:='0:25:00';
    PenaltyValue     : byte;    //fmGameOptions.seTimePenaltyValue.Value:=10;
    PenaltyCount     : byte;    //fmGameOptions.seTimePenaltyCount.Value:=10;
    GameLostByTime   : boolean; //fmGameOptions.cbLostByTime
    WordCheckMode    : byte;    //fmGameOptions.rgWordCheckMode.ItemIndex:=1;//Poll (0=Takeback, 2=Challenge)
    ChallengePenalty : byte;    //fmGameOptions.seChallengePenalty.Value:=10;//Challenge value
    ChallengeTime    : byte;    //fmGameOptions.seChallengePeriod.Value:=20; //Challenge value
    JokerExchange    : boolean; //fmGameOptions.cbCanJokerExchange.Checked:=false;
    ChangeIsPass     : boolean; //fmGameOptions.cbChangeIsPass.Checked:=false;
    CambioSecco      : boolean; //fmGameOptions.cbCambioSecco.Checked:=false;
    SubstractLetters : boolean; //fmGameOptions.cbSubstractLettersLeft.Checked:=true;
    AddLetters       : boolean; //fmGameOptions.cbAddLettersLeft.Checked:=true;
    JokerPenalty     : byte;    //fmGameOptions.seJokerPenalty.Value:=0;
    NumberOfPasses   : byte;    //fmGameOptions.seNumberOfPasses.Value:=3;
    LimitExchange    : byte;    //fmGameOptions.seLimitExchange.Value:=7;
    EndBonus         : byte;    //fmGameOptions.seGameEndBonus.Value:=0;
    ScrabbleBonus    : byte;    //fmGameOptions.seScrabbleEndBonus.Value:=50;
  end;


  { TDefaults }

  TDefaults=class
    private
      function GetCount: byte;
      function GetDefaultValue(aLanguage:TDefaultsLanguage): TDefaultValues;
      function GetDefaultValueByName(aLanguageName: string): TDefaultValues;
    public
      property Value[aLanguage:TDefaultsLanguage]:TDefaultValues read GetDefaultValue;
      property ValueByName[aLanguage:string]:TDefaultValues read GetDefaultValueByName;default;
      property Count:byte read GetCount;
    end;

  function LoadFlagFromResource(const aCountry: string; var aPng:TPortableNetworkGraphic):boolean;

var
  Defaults : TDefaults;

implementation

const
   UnknownSet:TDefaultValues=(
   English:'';
   Native:'';
   LangFile:'';
   DicFile:'';
   FlagID:'';
   Letters:'';
   LetterCount:'';
   LetterValue:'';
   NumberOfJokers:2;
   ReadingDirection:rdLeftToRight;
   ExcludedCat:'';
   RulesValid:false;
   NumberOfLetters:7;
   NumberOfRandoms:0;
   TimeControl:2;
   TimeControlEnd:false;
   TimeControlBuy:true;
   TimePerGame:'0:25:00';
   PenaltyValue:10;
   PenaltyCount:10;
   GameLostByTime:false;
   WordCheckMode:1;
   ChallengePenalty:10;
   ChallengeTime:20;
   JokerExchange:false;
   ChangeIsPass:true;
   CambioSecco:false;
   SubstractLetters:true;
   AddLetters:true;
   JokerPenalty:0;
   NumberOfPasses:3;
   LimitExchange:7;
   EndBonus:0;
   ScrabbleBonus:50);

  DefaultSettings:array[TDefaultsLanguage] of TDefaultValues=(
  {$I df_brazilian.inc},//pt_BR
  {$I df_catalan.inc},
  {$I df_dutch.inc},
  {$I df_american.inc},
  {$I df_british.inc},
  {$I df_finnish.inc},

  {$I df_french.inc},
  {$I df_german.inc},
  {$I df_greek.inc},
  {$I df_hebrew.inc},
  {$I df_irish.inc},

  {$I df_italian.inc},
  {$I df_latin.inc},
  {$I df_persian.inc},//farsi
  {$I df_portuguese.inc},//pt_PT
  {$I df_romanian.inc},
  {$I df_russian.inc},

  {$I df_scottish.inc},
  {$I df_slovak.inc},
  {$I df_spanish.inc},
  {$I df_swedish.inc},
  {$I df_turkish.inc}
);

function TDefaults.GetDefaultValueByName(aLanguageName: string): TDefaultValues;
begin
  case aLanguageName of
   'English (American English)':Result:=DefaultSettings[dfAmericanEnglish];
   'English (British English)':Result:=DefaultSettings[dfBritishEnglish];
   'Catalan (Català)':Result:=DefaultSettings[dfCatalan];
   'Dutch (Nederlands)':Result:=DefaultSettings[dfDutch];
   'Finnish (Suomi)':Result:=DefaultSettings[dfFinnish];
   'French (Français)':Result:=DefaultSettings[dfFrench];
   'German (Deutsch)':Result:=DefaultSettings[dfGerman];
   'Greek (Ελληνικά)':Result:=DefaultSettings[dfGreek];
   'Hebrew (עברית)':Result:=DefaultSettings[dfHebrew];
   'Irish (Gaeilge)':Result:=DefaultSettings[dfIrish];
   'Italian (Italiano)':Result:=DefaultSettings[dfItalian];
   'Latin (Lingua Latina)':Result:=DefaultSettings[dfLatin];
   'Persian (فارسی)':Result:=DefaultSettings[dfPersian];
   'Portuguese (Português europeu)':Result:=DefaultSettings[dfPortuguese];
   'Brazilian (Português brasileiro)':Result:=DefaultSettings[dfBrazilian];
   'Romanian (Română)':Result:=DefaultSettings[dfRomanian];
   'Russian (Русский)':Result:=DefaultSettings[dfRussian];
   'Scottish Gaelic (Gàidhlig)':Result:=DefaultSettings[dfScottish];
   'Slovak (Slovenčina)':Result:=DefaultSettings[dfSlovak];
   'Spanish (Español)':Result:=DefaultSettings[dfSpanish];
   'Swedish (Svenska)':Result:=DefaultSettings[dfSwedish];
   'Turkish (Türkçe)':Result:=DefaultSettings[dfTurkish];
   else Result:=UnknownSet;
  end;
end;

function TDefaults.GetCount: byte;
begin
  Result:=integer(high(TDefaultsLanguage));
end;

function TDefaults.GetDefaultValue(aLanguage: TDefaultsLanguage): TDefaultValues;
begin
  Result:=DefaultSettings[aLanguage];
end;

function LoadFlagFromResource(const aCountry: string; var aPng:TPortableNetworkGraphic):boolean;
var
  aTLD:string;
begin
  if pos('_',aCountry)>0 then
   case aCountry of
    'ca_ES':aTLD:='catalonia';
    'gd_GB':aTLD:='scotland';
    'eu_EU':aTLD:='europeanunion';
    else
     aTLD:=copy(aCountry,1,pos('_',aCountry)-1);
   end else
  case aCountry of
    'Afghanistan':aTLD:='AF';
    'Albania':aTLD:='AL';
    'Algeria':aTLD:='DZ';
    'American Samoa':aTLD:='AS';
    'Ancient Rome':aTLD:='Latin';
    'Andorra':aTLD:='AD';
    'Angola':aTLD:='AO';
    'Anguilla':aTLD:='AI';
//    'Antarctica':aTLD:='AQ';
    'Antigua and Barbuda':aTLD:='AG';
    'Argentina':aTLD:='AR';
    'Armenia':aTLD:='AM';
    'Aruba':aTLD:='AW';
    'Australia':aTLD:='AU';
    'Austria':aTLD:='AT';
    'Azerbaijan':aTLD:='AZ';
    'Bahrain':aTLD:='BH';
    'Bangladesh':aTLD:='BD';
    'Barbados':aTLD:='BB';
    'Belarus':aTLD:='BY';
    'Belgium':aTLD:='BE';
    'Belize':aTLD:='BZ';
    'Benin':aTLD:='BJ';
    'Bermuda':aTLD:='BM';
    'Bhutan':aTLD:='BT';
    'Bolivia':aTLD:='BO';
    'Bosnia and Herzegovina':aTLD:='BA';
    'Botswana':aTLD:='BW';
    'Bouvet Island':aTLD:='BV';
    'Brazil':aTLD:='BR';
    'British Indian Ocean Territory':aTLD:='IO';
    'British Virgin Islands':aTLD:='VG';
    'Brunei Darussalam':aTLD:='BN';
    'Bulgaria':aTLD:='BG';
    'Burkina Faso':aTLD:='BF';
    'Burundi':aTLD:='BI';
    'Cambodia':aTLD:='KH';
    'Cameroon':aTLD:='CM';
    'Canada':aTLD:='CA';
    'Catalonia':aTLD:='catalonia';  //non standard
    'Cape Verde':aTLD:='CV';
    'Cayman Islands':aTLD:='KY';
    'Central African Republic':aTLD:='CF';
    'Chad':aTLD:='TD';
    'Chile':aTLD:='CL';
    'China':aTLD:='CN';
    'Christmas Island':aTLD:='CX';
    'Cocos (Keeling) Islands':aTLD:='CC';
    'Colombia':aTLD:='CO';
    'Comoros':aTLD:='KM';
    'Congo':aTLD:='CG';
    'Cook Islands':aTLD:='CK';
    'Costa Rica':aTLD:='CR';
    'Cote D''Ivoire':aTLD:='CI';
    'Croatia':aTLD:='HR';
    'Cuba':aTLD:='CU';
    'Cyprus':aTLD:='CY';
    'Czech Republic':aTLD:='CZ';
    'Denmark':aTLD:='DK';
    'Djibouti':aTLD:='DJ';
    'Dominica':aTLD:='DM';
    'Dominican Republic':aTLD:='DO';
    'East Timor':aTLD:='TL';
//    'East Timor (Timor-Leste)':aTLD:='TP';
    'Ecuador':aTLD:='EC';
    'Egypt':aTLD:='EG';
    'El Salvador':aTLD:='SV';
    'England':aTLD:='england';
    'Equatorial Guinea':aTLD:='GQ';
    'Eritrea':aTLD:='ER';
    'Estonia':aTLD:='EE';
    'Ethiopia':aTLD:='ET';
    'European Union':aTLD:='europeanunion';  //non standard
    'Falkland Islands':aTLD:='FK';
    'Faroe Islands':aTLD:='FO';
    'Fiji':aTLD:='FJ';
    'Finland':aTLD:='FI';
    'France':aTLD:='FR';
    'French Guiana':aTLD:='GF';
    'French Polynesia':aTLD:='PF';
    'French Southern Territories':aTLD:='TF';
    'Gabon':aTLD:='GA';
    'Georgia':aTLD:='GE';
    'Germany':aTLD:='DE';
    'Ghana':aTLD:='GH';
    'Gibraltar':aTLD:='GI';
    'Greece':aTLD:='GR';
    'Greenland':aTLD:='GL';
    'Grenada':aTLD:='GD';
    'Guadeloupe':aTLD:='GP';
    'Guam':aTLD:='GU';
    'Guatemala':aTLD:='GT';
    'Guernsey and Alderney':aTLD:='GG';
    'Guinea':aTLD:='GN';
    'Guinea-Bissau':aTLD:='GW';
    'Guyana':aTLD:='GY';
    'Haiti':aTLD:='HT';
    'Heard and McDonald Island':aTLD:='HM';
    'Honduras':aTLD:='HN';
    'Hong Kong':aTLD:='HK';
    'Hungary':aTLD:='HU';
    'Iceland':aTLD:='IS';
    'India':aTLD:='IN';
    'Indonesia':aTLD:='ID';
    'Iran':aTLD:='IR';
    'Iraq':aTLD:='IQ';
    'Ireland':aTLD:='IE';
    'Isle of Man':aTLD:='IM';
    'Israel':aTLD:='IL';
    'Italy':aTLD:='IT';
    'Jamaica':aTLD:='JM';
    'Japan':aTLD:='JP';
    'Jersey':aTLD:='JE';
    'Jordan':aTLD:='JO';
    'Kazakhstan':aTLD:='KZ';
    'Kenya':aTLD:='KE';
    'Kiribati':aTLD:='KI';
    'Kosovo':aTLD:='KV';
    'Kuwait':aTLD:='KW';
    'Kyrgyzstan':aTLD:='KG';
    'Laos':aTLD:='LA';
    'Latvia':aTLD:='LV';
    'Lebanon':aTLD:='LB';
    'Lesotho':aTLD:='LS';
    'Liberia':aTLD:='LR';
    'Libya':aTLD:='LY';
    'Liechtenstein':aTLD:='LI';
    'Lithuania':aTLD:='LT';
    'Luxembourg':aTLD:='LU';
    'Macao':aTLD:='MO';
    'Macedonia':aTLD:='MK';
    'Madagascar':aTLD:='MG';
    'Malawi':aTLD:='MW';
    'Malaysia':aTLD:='MY';
    'Maldives':aTLD:='MV';
    'Mali':aTLD:='ML';
    'Malta':aTLD:='MT';
    'Marshall Islands':aTLD:='MH';
    'Martinique':aTLD:='MQ';
    'Mauritania':aTLD:='MR';
    'Mauritius':aTLD:='MU';
    'Mayotte':aTLD:='YT';
    'Mexico':aTLD:='MX';
    'Micronesia':aTLD:='FM';
    'Moldova':aTLD:='MD';
    'Monaco':aTLD:='MC';
    'Mongolia':aTLD:='MN';
    'Montenegro':aTLD:='CS';
    'Montserrat':aTLD:='MS';
    'Morocco':aTLD:='MA';
    'Mozambique':aTLD:='MZ';
    'Myanmar':aTLD:='MM';
    'Namibia':aTLD:='NA';
    'Nauru':aTLD:='NR';
    'Nepal':aTLD:='NP';
    'Netherlands':aTLD:='NL';
    'Netherlands Antilles':aTLD:='AN';
    'New Caledonia':aTLD:='NC';
    'New Zealand':aTLD:='NZ';
    'Nicaragua':aTLD:='NI';
    'Niger':aTLD:='NE';
    'Nigeria':aTLD:='NG';
    'Niue':aTLD:='NU';
    'Norfolk Island':aTLD:='NF';
    'North Korea':aTLD:='KP';
    'Northern Mariana Islands':aTLD:='MP';
    'Norway':aTLD:='NO';
    'Oman':aTLD:='OM';
    'Pakistan':aTLD:='PK';
    'Palau':aTLD:='PW';
    'Palestinian Territory':aTLD:='PS';
    'Panama':aTLD:='PA';
    'Papua New Guinea':aTLD:='PG';
    'Paraguay':aTLD:='PY';
    'Peru':aTLD:='PE';
    'Philippines':aTLD:='PH';
    'Pitcairn Island':aTLD:='PN';
    'Poland':aTLD:='PL';
    'Portugal':aTLD:='PT';
    'Puerto Rico':aTLD:='PR';
    'Qatar':aTLD:='QA';
    'Reunion':aTLD:='RE';
    'Romania':aTLD:='RO';
    'Russia':aTLD:='RU';
    'Rwanda':aTLD:='RW';
    'Saint Barthelemy':aTLD:='FR';//aTLD:='BL'; same flag as france
    'Saint Helena':aTLD:='SH';
    'Saint Kitts and Nevis':aTLD:='KN';
    'Saint Lucia':aTLD:='LC';
    'Saint Martin':aTLD:='FR';//aTLD:='MF'; same flag as france
    'Saint Vincent and the Grenadines':aTLD:='VC';
    'Saint-Pierre und Miquelon':aTLD:='PM';
    'Samoa':aTLD:='WS';
    'San Marino':aTLD:='SM';
    'Sao Tome and Principe':aTLD:='ST';
    'Saudi Arabia':aTLD:='SA';
    'Scotland':aTLD:='scotland';   //non standard
    'Senegal':aTLD:='SN';
    'Serbia':aTLD:='RS';
    'Seychelles':aTLD:='SC';
    'Sierra Leone':aTLD:='SL';
    'Singapore':aTLD:='SG';
    'Slovakia':aTLD:='SK';
    'Slovenia':aTLD:='SI';
    'Solomon Islands':aTLD:='SB';
    'Somalia':aTLD:='SO';
    'South Africa':aTLD:='ZA';
    'South Georgia and the South Sandwich Islands':aTLD:='GS';
    'South Korea':aTLD:='KR';
    'South Sudan':aTLD:='SS';
    'Spain':aTLD:='ES';
    'Sri Lanka':aTLD:='LK';
    'Sudan':aTLD:='SD';
    'Suriname':aTLD:='SR';
    'Svalbard and Jan Mayen':aTLD:='SJ';
    'Swaziland':aTLD:='SZ';
    'Sweden':aTLD:='SE';
    'Switzerland':aTLD:='CH';
    'Syrian Arab Republic':aTLD:='SY';
    'Taiwan':aTLD:='TW';
    'Tajikistan':aTLD:='TJ';
    'Tanzania':aTLD:='TZ';
    'Thailand':aTLD:='TH';
    'The Bahamas':aTLD:='BS';
    'The Gambia':aTLD:='GM';
    'Togo':aTLD:='TG';
    'Tokelau':aTLD:='TK';
    'Tonga':aTLD:='TO';
    'Trinidad and Tobago':aTLD:='TT';
    'Tunisia':aTLD:='TN';
    'Turkey':aTLD:='TR';
    'Turkmenistan':aTLD:='TM';
    'Turks and Caicos Islands':aTLD:='TC';
    'Tuvalu':aTLD:='TV';
    'Uganda':aTLD:='UG';
    'Ukraine':aTLD:='UA';
    'United Arab Emirates':aTLD:='AE';
    'United Kingdom':aTLD:='gb';//aTLD:='UK';
    'United States':aTLD:='US';
    'United States Minor Outlying Islands':aTLD:='UM';
    'United States Virgin Islands':aTLD:='VI';
    'Uruguay':aTLD:='UY';
    'Uzbekistan':aTLD:='UZ';
    'Vanuatu':aTLD:='VU';
    'Vatican':aTLD:='VA';
    'Venezuela':aTLD:='VE';
    'Viet Nam':aTLD:='VN';
    'Wales':aTLD:='wales';
    'Wallis and Futuna':aTLD:='WF';
    'Western Sahara':aTLD:='EH';
    'Yemen':aTLD:='YE';
    'Zambia':aTLD:='ZM';
    'Zimbabwe':aTLD:='ZW';
    else
     aTLD:=aCountry;
  end; //case
  if LazarusResources.Find(aTLD)<>nil then
  begin
    aPng.LoadFromLazarusResource(aTLD);
    Result:=true;
  end else
    Result:=false;
end;

initialization
  Defaults:=TDefaults.Create;
finalization
  Defaults.Free;
end.

