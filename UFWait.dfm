object FWait: TFWait
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  ClientHeight = 295
  ClientWidth = 369
  Color = 3288877
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object LNowShrinking: TLabel
    Left = 0
    Top = 245
    Width = 369
    Height = 50
    Align = alBottom
    Alignment = taCenter
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Layout = tlCenter
    WordWrap = True
    ExplicitTop = 70
  end
  object LTotalClasses: TLabel
    Left = 35
    Top = 84
    Width = 300
    Height = 21
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object LUsedClasses: TLabel
    Left = 35
    Top = 109
    Width = 300
    Height = 21
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object LMessage: TLabel
    Left = 35
    Top = 134
    Width = 300
    Height = 47
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object ASPBJNI: TProgressBar
    Left = 37
    Top = 212
    Width = 300
    Height = 17
    BarColor = clBlue
    BackgroundColor = clBlack
    TabOrder = 0
    TabStop = True
  end
  object ASPB: TProgressBar
    Left = 37
    Top = 54
    Width = 300
    Height = 17
    BarColor = clBlue
    BackgroundColor = clBlack
    TabOrder = 1
    TabStop = True
  end
end
