{$define UseOpenGl}

{$define UseRegEx}  //regex does not work in ide for windows

{.$define Portable}

{.$define TestServer}

{.$define Debug}

{$ifdef Debug}
{.$define DebugRepaint}
{.$define DebugNetworkFull}
{.$define DebugNetwork}
{$define DebugStatusBar}
{$endif}
