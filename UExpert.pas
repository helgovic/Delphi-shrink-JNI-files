unit UExpert;

interface

uses
  Windows, SysUtils, Classes, vcl.Menus, vcl.ActnList, ToolsAPI, vcl.ComCtrls, vcl.ExtCtrls, vcl.Graphics, vcl.Controls,
  System.IOUtils, vcl.Dialogs, Threading, vcl.Forms, vcl.VirtualImageList, Vcl.ImageCollection;

type

  TGraphicHack = class(TGraphic);

  TBADIToolsAPIFunctions = record
     Class Procedure RegisterFormClassForTheming(Const AFormClass : TCustomFormClass;
        Const Component : TComponent = Nil); static;
  end;

  TJavaLoginMethodsExpert = class(TObject)
  private
    { Private declarations }
    FProjectMenu,
    FMenuShrinkAll: TMenuItem;
    FActionShrinkAll: TAction;
    FMenuCurrShrink: TMenuItem;
    FActionCurrShrink: TAction;
    FMenuUnShrinkAll: TMenuItem;
    FActionUnShrinkAll: TAction;
    FMenuCurrUnShrink: TMenuItem;
    FActionCurrUnShrink: TAction;
    procedure ShrinkAllExecute(Sender: TObject);
    procedure CurrShrinkExecute(Sender: TObject);
    procedure CurrUnShrinkExecute(Sender: TObject);
    procedure UnShrinkAllExecute(Sender: TObject);
    procedure ShrinkFile(FileName: String);
  protected
    { Protected declarations }
    function AddAction(ACaption, AHint, AName : String; AExecuteEvent,
      AUpdateEvent : TNotifyEvent) : TAction;
    procedure RemoveAction(AAction: TAction; AToolbar: TToolbar);
    procedure RemoveActionFromToolbar(AAction: TAction);
  public
    { Public declarations }
    constructor Create; virtual;
    destructor Destroy; override;
    class function Instance: TJavaLoginMethodsExpert;
  public
  end;

  function ShrinkJNIExpert: TJavaLoginMethodsExpert;

implementation

uses
   JclStrings, UFWait;

var
   FJavaLogInMethodsExpert: TJavaLoginMethodsExpert;
   JNIFiles: array of string;
   FileList: TArray<String>;

{$IFDEF VER360}
{$ELSEIF VER350}
{$ELSE}
function AddIconToImageList(AIcon: TIcon; ImageList: TCustomImageList;
  Stretch: Boolean): Integer;
const
  MaskColor = clBtnFace;
var
  SrcBmp, DstBmp: TBitmap;
  PSrc1, PSrc2, PDst: PRGBArray;
  X, Y: Integer;
begin
  Assert(Assigned(AIcon));
  Assert(Assigned(ImageList));

{$IFDEF DEBUG}
  if not AIcon.Empty then
    CnDebugger.LogFmt('AddIcon %dx%d To ImageList %dx%d', [AIcon.Width, AIcon.Height,
      ImageList.Width, ImageList.Height]);
{$ENDIF}

  if (ImageList.Width = 16) and (ImageList.Height = 16) and not AIcon.Empty and
    (AIcon.Width = 32) and (AIcon.Height = 32) then
  begin
    if Stretch then // ImageList 尺寸比图标大，指定拉伸的情况下，使用平滑处理
    begin
      SrcBmp := nil;
      DstBmp := nil;
      try
        SrcBmp := CreateEmptyBmp24(32, 32, MaskColor);
        DstBmp := CreateEmptyBmp24(16, 16, MaskColor);
        SrcBmp.Canvas.Draw(0, 0, AIcon);
        for Y := 0 to DstBmp.Height - 1 do
        begin
          PSrc1 := SrcBmp.ScanLine[Y * 2];
          PSrc2 := SrcBmp.ScanLine[Y * 2 + 1];
          PDst := DstBmp.ScanLine[Y];
          for X := 0 to DstBmp.Width - 1 do
          begin
            PDst^[X].b := (PSrc1^[X * 2].b + PSrc1^[X * 2 + 1].b + PSrc2^[X * 2].b
              + PSrc2^[X * 2 + 1].b) shr 2;
            PDst^[X].g := (PSrc1^[X * 2].g + PSrc1^[X * 2 + 1].g + PSrc2^[X * 2].g
              + PSrc2^[X * 2 + 1].g) shr 2;
            PDst^[X].r := (PSrc1^[X * 2].r + PSrc1^[X * 2 + 1].r + PSrc2^[X * 2].r
              + PSrc2^[X * 2 + 1].r) shr 2;
          end;
        end;
        Result := ImageList.AddMasked(DstBmp, MaskColor);
      finally
        if Assigned(SrcBmp) then FreeAndNil(SrcBmp);
        if Assigned(DstBmp) then FreeAndNil(DstBmp);
      end;
    end
    else
    begin
      // 指定不拉伸的情况下，把 32*32 图标的左上角 16*16 部分绘制来加入
      DstBmp := nil;
      try
        DstBmp := CreateEmptyBmp24(16, 16, MaskColor);
        DstBmp.Canvas.Draw(0, 0, AIcon);
        Result := ImageList.AddMasked(DstBmp, MaskColor);
      finally
        DstBmp.Free;
      end;
    end;
  end
  else if not AIcon.Empty then
    Result := ImageList.AddIcon(AIcon)
  else
    Result := -1;
end;

{$ENDIF}

function GetCurrentEditorFileName: string;
var
  Module: IOTAModule;
  Editor: IOTAEditor;
begin
  Result := '';
  Module := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
  if Assigned(Module) then
  begin
    Editor := Module.CurrentEditor;
    if Assigned(Editor) then
      Result := Editor.FileName;
  end;
end;

function StrInArray(const Value : String;const ArrayOfString : Array of String) : Boolean;
var
 Loop : String;
begin
  for Loop in ArrayOfString do
  begin
    if Value = Loop then
    begin
       Exit(true);
    end;
  end;
  result := false;
end;

function FoundInFile(FilePath, SearchStr: string): Boolean;

var
  sl: TStringList;
  line: string;

begin

   Result := False;
   sl := TStringList.Create;

   try

      sl.LoadFromFile(FilePath);

      for line in sl do
        if Pos(AnsiLowerCase(SearchStr), AnsiLowerCase(Line)) <> 0
        then
           begin
             Result := True;
             Break;
           end;

   finally
      sl.Free;
   end;

end;

function GetProjectGroup: IOTAProjectGroup;

var
   IModuleServices: IOTAModuleServices;
   IModule: IOTAModule;
   i: Integer;

begin

   IModuleServices := BorlandIDEServices as IOTAModuleServices;

   Result := nil;

   for i := 0 to IModuleServices.ModuleCount - 1 do
      begin

         IModule := IModuleServices.Modules[i];

         if IModule.QueryInterface(IOTAProjectGroup, Result) = S_OK
         then
            Break;

      end;

end;

function GetCurrentProject: IOTAProject;

var
   Project: IOTAProject;
   ProjectGroup: IOTAProjectGroup;

begin

   Result := nil;

   ProjectGroup := GetProjectGroup;

   if Assigned(ProjectGroup)
   then
      begin

         Project := ProjectGroup.ActiveProject;

         if Assigned(Project)
         then
            Result := Project;

      end;

end;

function GetCurrentProjectFileName: string;

var
  IProject: IOTAProject;
begin
  Result := '';

  IProject := GetCurrentProject;
  if Assigned(IProject) then
  begin
    Result := IProject.FileName;
  end;
end;

function FindMenuItem(MenuCaptions: String): TMenuItem;

var
   Captions: TStringList;
   NTAServices: INTAServices;
   y, i: integer;
   MenuItems: TMenuItem;
   Caption: String;
   Found: Boolean;

begin

   Result := nil;

   if Supports(BorlandIDEServices, INTAServices, NTAServices)
   then
      begin

         Captions := TStringList.Create;
         Captions.Delimiter := ';';
         Captions.StrictDelimiter := True;
         Captions.DelimitedText := MenuCaptions;

         MenuItems := NTAServices.MainMenu.Items;

         for y := 0 to Captions.Count - 1 do
            begin

               Found := False;

               for i := 0 to MenuItems.Count - 1 do
                  begin

                     Caption := StringReplace(MenuItems.Items[i].Caption, '&', '', []);

                     if Uppercase(Caption) = Uppercase(Captions[y])
                     then
                        begin
                           MenuItems := MenuItems.Items[i];
                           Found := True;
                           Break;
                        end;

                  end;

               if not Found
               then
                  begin
                     Captions.Free;
                     Exit;
                  end;

            end;

         Result := MenuItems;
         Captions.Free;

      end;

end;

function AddGraphicToVirtualImageList(Graphic: TGraphic; DstVirtual: TVirtualImageList;
  const ANamePrefix: string; Disabled: Boolean): Integer;
var
  C: Integer;
  R: TRect;
  Bmp: TBitmap;
  Mem: TMemoryStream;
  Collection: TImageCollection;
begin
  Result := -1;
  if (Graphic = nil) or (DstVirtual = nil) then
    Exit;

  if DstVirtual.ImageCollection is TImageCollection then
    Collection := DstVirtual.ImageCollection as TImageCollection
  else
    Exit;

  C := Collection.Count;
  Mem := TMemoryStream.Create;
  try
    if Graphic is TIcon then // 是 Icon 则直接存避免丢失透明度
    begin
      Mem.Clear;
      (Graphic as TIcon).SaveToStream(Mem);
    end
    else if Graphic is TBitmap then
    begin
      Mem.Clear;
      (Graphic as TBitmap).SaveToStream(Mem);
    end
    else
    begin
      Bmp := TBitmap.Create;
      try
        Bmp.PixelFormat := pf32bit;
        Bmp.AlphaFormat := afIgnored;
        Bmp.Width := Graphic.Width;
        Bmp.Height := Graphic.Height;
        R := Rect(0, 0, Bmp.Width, Bmp.Height);
        TGraphicHack(Graphic).Draw(Bmp.Canvas, R);

        Mem.Clear;
        Bmp.SaveToStream(Mem);
      finally
        Bmp.Free;
      end;
    end;
    Collection.Add(ANamePrefix + IntToStr(C), Mem);
  finally
    Mem.Free;
  end;

  DstVirtual.Add('', C, C, Disabled);
  Result := DstVirtual.Count - 1;
end;

procedure TJavaLoginMethodsExpert.UnShrinkAllExecute(Sender: TObject);
begin

   FWait.Show;

   TThread.CreateAnonymousThread(
   procedure

   var
      i, x: integer;
      FileModule: IOTAModule;

   begin

      FileList := TDirectory.GetFiles(ExtractFilePath(GetCurrentProjectFileName), '*.pas', TSearchOption.soTopDirectoryOnly);

      JNIFiles := nil;

      for x := 0 to High(FileList) do
         begin

            if FoundInFile(FileList[x], '[JavaSignature(')
            then
               begin

                  SetLength(JNIFiles, Length(JNIFiles) + 1);
                  JNIFiles[High(JNIFiles)] := FileList[x];

               end;

         end;

      TThread.Synchronize(TThread.CurrentThread,
      procedure
      begin
         FWait.ASPBJNI.Max := Length(JNIFiles);
         FWait.ASPBJNI.Position := 0;
      end);

      for i := 0 to High(JNIFiles) do
         begin

            TThread.Synchronize(TThread.CurrentThread,
            procedure
            begin
               FWait.ASPBJNI.Position := i;
               FWait.Refresh;
            end);

            if FileExists(StrBefore('.pas', JNIFiles[i]) + 'Full.pas')
            then
               begin

                  TThread.Synchronize(TThread.CurrentThread,
                  procedure
                  begin
                     FWait.LNowShrinking.Caption := 'Unshrinking: ' + JNIFiles[i];
                     FWait.Refresh;
                  end);

                  DeleteFile(JNIFiles[i]);
                  RenameFile(StrBefore('.pas', JNIFiles[i]) + 'Full.pas', JNIFiles[i]);

                  with BorlandIDEServices as IOTAModuleServices do
                     begin

                        FileModule := FindModule(JNIFiles[i]);

                        if Assigned(FileModule)
                        then
                           FileModule.Refresh(True);

                     end;

               end;

         end;

      FWait.Close;

   end).Start;

end;

procedure TJavaLoginMethodsExpert.CurrUnShrinkExecute(Sender: TObject);

var
   FileName: String;
   FileModule: IOTAModule;

begin

   FileName := GetCurrentEditorFileName;

   if not FileExists(StrBefore('.pas', FileName) + 'Full.pas')
   then
      begin
         ShowMessage('File is not shrunk');
         Exit;
      end;

   FWait.Show;
   FWait.ASPBJNI.Position := 0;

   DeleteFile(FileName);
   RenameFile(StrBefore('.pas', FileName) + 'Full.pas', FileName);

   with BorlandIDEServices as IOTAModuleServices do
      begin

         FileModule := FindModule(FileName);

         if Assigned(FileModule)
         then
            FileModule.Refresh(True);

      end;

   FWait.Close;

end;

procedure TJavaLoginMethodsExpert.ShrinkFile(FileName: String);

var
   x, y, z: integer;
   PasFile: TextFile;
   PasFileOut: TextFile;
   Line: String;
   JNIClasses, ClassesUsed: array of string;
   sl: TStringList;
   ClassesFound: Boolean;
   FirstClass, LastClass, Offset: integer;
   TmpStr, TmpStr2: string;
   FileModule: IOTAModule;

begin

   TThread.Synchronize(TThread.CurrentThread,
   procedure
   begin
      FWait.LNowShrinking.Caption := 'Shrinking ' + FileName;
      FWait.LUsedClasses.Caption := '';
      FWait.Refresh;
   end);

   sl := TStringList.Create;

   JNIClasses := nil;

   AssignFile(PasFile, FileName);
   Reset(PasFile);

   while not Eof(PasFile) do
      begin

         Readln(PasFile, Line);

         if Pos('Class = interface(', Line) > 0
         then
            Break;

         if Pos(' = interface;', Line) > 0
         then
            begin
               SetLength(JNIClasses, Length(JNIClasses) + 1);
               JNIClasses[High(JNIClasses)] := AnsiLowerCase(Trim(StrBefore(' = interface;', Line)));
            end;

      end;

   CloseFile(PasFile);

   TThread.Synchronize(TThread.CurrentThread,
   procedure
   begin
      FWait.LTotalClasses.Caption := 'Total number of classes: ' + IntToStr(Length(JNIClasses));
   end);

   TThread.Synchronize(TThread.CurrentThread,
   procedure
   begin
      FWait.ASPB.Max := High(FileList);
      FWait.ASPB.Position := 0;
   end);

   for x := 0 to High(FileList) do
      begin

         if (not StrInArray(FileList[x], JNIFiles)) and
            (FoundInFile(FileList[x], StrBefore('.pas', ExtractFileName(FileName))))
         then
            begin

               TThread.Synchronize(TThread.CurrentThread,
               procedure
               begin
                  FWait.LMessage.Caption := 'Processing File: ' + FileList[x];
               end);

               sl.LoadFromFile(FileList[x]);

               for z := 0 to sl.Count - 1 do
                  begin

                     for y := 0 to High(JNIClasses) do
                        begin

                           if Pos(JNIClasses[y], AnsiLowerCase(sl[z])) > 0
                           then
                              begin

                                 if not StrInArray(JNIClasses[y], ClassesUsed)
                                 then
                                    begin
                                       SetLength(ClassesUsed, Length(ClassesUsed) + 1);
                                       ClassesUsed[High(ClassesUsed)] := JNIClasses[y];
                                    end;

                              end;

                        end;
                  end;

            end;

         TThread.Synchronize(TThread.CurrentThread,
         procedure
         begin
            FWait.ASPB.Position := x + 1;
         end);

      end;

   if ClassesUsed = nil
   then
      Exit;

   FirstClass := 0;
   LastClass := High(ClassesUsed);

   ClassesFound := True;

   try

      sl.LoadFromFile(FileName);

      while ClassesFound do
         begin

            TThread.Synchronize(TThread.CurrentThread,
            procedure
            begin
               FWait.LUsedClasses.Caption := 'Classes Used: ' + IntToStr(Length(ClassesUsed));
            end);

            ClassesFound := False;

            y := 0;

            while y <= sl.Count - 1 do
               begin

                  if Pos('class = interface(', AnsiLowerCase(sl[y])) > 0
                  then
                     for x := FirstClass to LastClass do
                        if ClassesUsed[x] = Trim(StrBefore('class = interface(', AnsiLowerCase(sl[y])))
                        then
                           begin

                              if (not StrInArray(AnsiLowerCase(StrBefore('class)', StrAfter('class = interface(', AnsiLowerCase(sl[y])))), ClassesUsed))
                              then
                                 begin

                                    SetLength(ClassesUsed, Length(ClassesUsed) + 1);
                                    ClassesUsed[High(ClassesUsed)] := AnsiLowerCase(StrBefore('class)', StrAfter('class = interface(', AnsiLowerCase(sl[y]))));
                                    ClassesFound := True;

                                    Inc(y);

                                 end;

                              TmpStr2 := 't' + ClassesUsed[x];

                              while Pos(TmpStr2, AnsiLowerCase(sl[y])) = 0 do
                                 begin

                                    Offset := StrFind(': J', sl[y], 1);

                                    while Offset > 0 do
                                       begin

                                          z := Offset + 2;
                                          TmpStr := '';

                                          while (sl[y][z] <> ';') and (sl[y][z] <> ')') do
                                             begin
                                                TmpStr := TmpStr + sl[y][z];
                                                Inc(z);
                                             end;

                                          if (not StrInArray(AnsiLowerCase(TmpStr), ClassesUsed)) and
                                             (StrInArray(AnsiLowerCase(TmpStr), JNIClasses))
                                          then
                                             begin
                                                SetLength(ClassesUsed, Length(ClassesUsed) + 1);
                                                ClassesUsed[High(ClassesUsed)] := AnsiLowerCase(TmpStr);
                                                ClassesFound := True;
                                             end;

                                          Offset := StrFind(': J', sl[y], z + 1);

                                       end;

                                    Offset := StrFind('<', sl[y], 1);

                                    while Offset > 0 do
                                       begin

                                          z := Offset + 1;
                                          TmpStr := '';

                                          while (sl[y][z] <> '>') do
                                             begin
                                                TmpStr := TmpStr + sl[y][z];
                                                Inc(z);
                                             end;

                                          if (not StrInArray(AnsiLowerCase(TmpStr), ClassesUsed)) and
                                             (StrInArray(AnsiLowerCase(TmpStr), JNIClasses))
                                          then
                                             begin
                                                SetLength(ClassesUsed, Length(ClassesUsed) + 1);
                                                ClassesUsed[High(ClassesUsed)] := AnsiLowerCase(TmpStr);
                                                ClassesFound := True;
                                             end;

                                          Offset := StrFind('<', sl[y], z);

                                       end;

                                    Inc(y);

                                 end;

                           end;

                  Inc(y);

               end;

            FirstClass := LastClass + 1;
            LastClass := High(ClassesUsed);

         end;

   except
         TThread.Synchronize(TThread.CurrentThread,
         procedure
         begin
            ShowMessage('Error LineNo = ' + y.ToString + 'class = ' + ClassesUsed[x]);
         end);
   end;

   try

      AssignFile(PasFileOut, StrBefore('.pas', FileName) + 'SH.pas');
      Rewrite(PasFileOut);

      x := 0;

      while (x <= sl.Count - 1) and (Pos(' = interface;', sl[x]) = 0) do
         begin
            Writeln(PasFileOut, sl[x]);
            Inc(x);
         end;

      while (x <= sl.Count - 1) and (Pos('class = interface(', AnsiLowerCase(sl[x])) = 0) do
         begin

            if (Pos(' = interface;', sl[x]) > 0)
            then
               begin

                  if (StrInArray(AnsiLowerCase(Trim(StrBefore(' = interface;', Sl[x]))), ClassesUsed))
                  then
                     Writeln(PasFileOut, sl[x]);

               end
            else
               Writeln(PasFileOut, sl[x]);

            Inc(x);

         end;

      while (x <= sl.Count - 1) and (Trim(sl[x]) <> 'implementation') do
         begin

            if (Pos('class = interface(', AnsiLowerCase(sl[x])) > 0) and
               (not Trim(sl[x]).StartsWith('//'))
            then
               begin

                  TmpStr := 't' + AnsiLowerCase(Trim(StrBefore('class = interface(', AnsiLowerCase(Sl[x]))));

                  if (StrInArray(AnsiLowerCase(Trim(StrBefore('class = interface(', AnsiLowerCase(Sl[x])))), ClassesUsed))
                  then
                     begin

                        while Pos(TmpStr, AnsiLowerCase(Sl[x])) = 0 do
                            begin
                               Writeln(PasFileOut, sl[x]);
                               Inc(x);
                            end;

                        Writeln(PasFileOut, sl[x]);
                        Inc(x);

                        while (x <= sl.Count - 1) and (Trim(sl[x]) <> '') do
                           begin
                              Writeln(PasFileOut, sl[x]);
                              Inc(x);
                           end;

                         Writeln(PasFileOut, sl[x]);

                     end
                  else
                     begin

                         while Pos(TmpStr, AnsiLowerCase(Sl[x])) = 0 do
                            Inc(x);

                        Inc(x);

                         while (x <= sl.Count - 1) and (Trim(sl[x]) <> '') do
                            Inc(x);

                     end

               end
            else
               if (Pos('//', sl[x]) > 0) and
                  (Trim(StrBefore('//', sl[x])) = '')
               then
               else
                  Writeln(PasFileOut, sl[x]);

            Inc(x);

            if x = sl.count - 1
            then
               Writeln(PasFileOut, sl[x]);

         end;

      while (x <= sl.Count - 1) and (Pos('TRegTypes.RegisterType(', sl[x]) = 0)  do
         begin
            Writeln(PasFileOut, sl[x]);
            Inc(x);
         end;

      if x >= sl.count - 1
      then
         ShowMessage('EOF : ' + FileName);

      while (x <= sl.Count - 1) and (Trim(sl[x]) <> 'initialization') do
         begin

            if (Pos('TRegTypes.RegisterType(', sl[x]) > 0)
            then
               begin

                  TmpStr := AnsiLowerCase(StrBefore(''',', StrAfter(StrBefore('.pas', ExtractFileName(FileName)) + '.', sl[x])));
                  if StrInArray(TmpStr, ClassesUsed)
                  then
                     Writeln(PasFileOut, sl[x]);

               end
            else
               Writeln(PasFileOut, sl[x]);

            Inc(x);

         end;

      while x <= sl.Count - 1 do
         begin
            Writeln(PasFileOut, sl[x]);
            Inc(x);
         end;

   except
      TThread.Synchronize(TThread.CurrentThread,
      procedure
      begin
         ShowMessage('Error LineNo = ' + x.ToString + 'class = ' + TmpStr);
      end);
   end;

   CloseFile(PasFileOut);
   sl.Free;

   RenameFile(FileName, StrBefore('.pas', FileName) + 'Full.pas');
   RenameFile(StrBefore('.pas', FileName) + 'SH.pas', FileName);

   with BorlandIDEServices as IOTAModuleServices do
      begin

         FileModule := FindModule(FileName);

         if Assigned(FileModule)
         then
            FileModule.Refresh(True);

      end;

end;

procedure TJavaLoginMethodsExpert.CurrShrinkExecute(Sender: TObject);
begin

   FWait.Show;
   FWait.ASPBJNI.Position := 0;

   TThread.CreateAnonymousThread(
   procedure

   var
      i: integer;
      FileName: string;

   begin

      try

         FileName := GetCurrentEditorFileName;

         if not FoundInFile(FileName, '[JavaSignature(')
         then
            Exit;

         if FileExists(StrBefore('.pas', FileName) + 'Full.pas')
         then
            Exit;

         FileList := TDirectory.GetFiles(ExtractFilePath(GetCurrentProjectFileName), '*.pas', TSearchOption.soTopDirectoryOnly);

         JNIFiles := nil;

         for i := 0 to High(FileList) do
            begin

               if FoundInFile(FileList[i], '[JavaSignature(')
               then
                  begin

                     SetLength(JNIFiles, Length(JNIFiles) + 1);
                     JNIFiles[High(JNIFiles)] := FileList[i];

                  end;

            end;

         ShrinkFile(FileName);

      finally
         FWait.Close;
      end;

   end).Start;

end;

function ShrinkJNIExpert: TJavaLoginMethodsExpert;
begin
  Result := TJavaLoginMethodsExpert.Instance;
end;

class function TJavaLoginMethodsExpert.Instance: TJavaLoginMethodsExpert;
begin
  if FJavaLogInMethodsExpert = nil then
    FJavaLogInMethodsExpert := TJavaLoginMethodsExpert.Create;
  Result := FJavaLogInMethodsExpert;
end;

procedure TJavaLoginMethodsExpert.ShrinkAllExecute(Sender: TObject);
begin

   FWait.Show;

   TThread.CreateAnonymousThread(
   procedure

   var
      i: integer;

   begin

      FileList := TDirectory.GetFiles(ExtractFilePath(GetCurrentProjectFileName), '*.pas', TSearchOption.soTopDirectoryOnly);

      JNIFiles := nil;

      for i := 0 to High(FileList) do
         begin

            if Pos('full.pas', AnsiLowerCase(FileList[i])) > 0
            then
               Continue;

            if FoundInFile(FileList[i], '[JavaSignature(')
            then
               begin

                  SetLength(JNIFiles, Length(JNIFiles) + 1);
                  JNIFiles[High(JNIFiles)] := FileList[i];

               end;

         end;

      TThread.Synchronize(TThread.CurrentThread,
      procedure
      begin
         FWait.ASPBJNI.Max := Length(JNIFiles);
         FWait.ASPBJNI.Position := 0;
      end);

      for i := 0 to High(JNIFiles) do
         if not FileExists(StrBefore('.pas', JNIFiles[i]) + 'Full.pas')
         then
            begin

               ShrinkFile(JNIFiles[i]);

               TThread.Synchronize(TThread.CurrentThread,
               procedure
               begin
                  FWait.ASPBJNI.Position := i;
               end);

            end;

      FWait.Close;

   end).Start;

end;

constructor TJavaLoginMethodsExpert.Create;

var
   NTAServices : INTAServices;
   Bmp: TBitmap;
   ImageIndex: integer;

begin

  inherited Create;

  { Main menu item }
   if Supports(BorlandIDEServices, INTAServices, NTAServices)
   then
      begin

         FProjectMenu := FindMenuItem('Project');

         Bmp := TBitmap.Create;

         FActionShrinkAll := TAction.Create(nil);
         FActionShrinkAll.Category := 'Project';
         FActionShrinkAll.Caption := 'Shrink JNI Files';
         FActionShrinkAll.Hint := 'Project Shrink JNI Files';
         FActionShrinkAll.Name := 'ShrinkAllAction';
         FActionShrinkAll.Visible := True;
         FActionShrinkAll.OnExecute := ShrinkAllExecute;
         FActionShrinkAll.Enabled := True;

         FMenuShrinkAll := TMenuItem.Create(nil);
         FMenuShrinkAll.Name := 'ShrinkAll';
         FMenuShrinkAll.Caption := 'Shrink JNI Files';
         FMenuShrinkAll.AutoHotkeys := maAutomatic;
         FMenuShrinkAll.Action := FActionShrinkAll;

         NTAServices.AddActionMenu(FProjectMenu.Name, FActionShrinkAll, FMenuShrinkAll, False, True);

         Bmp.LoadFromResourceName(HInstance, 'ShrinkAllBmp');

         {$IFDEF VER360}
             ImageIndex := AddGraphicToVirtualImageList(bmp, NTAServices.ImageList as TVirtualImageList, '', False);
         {$ELSEIF VER350}
             ImageIndex := AddGraphicToVirtualImageList(bmp, NTAServices.ImageList as TVirtualImageList, '', False);
         {$ELSE}
             ImageIndex := AddIconToImageList(AWizAction.FIcon, Svcs40.ImageList, False);
         {$ENDIF}

         FActionShrinkAll.ImageIndex := ImageIndex;
         FMenuShrinkAll.ImageIndex := ImageIndex;

         FActionCurrShrink := TAction.Create(nil);
         FActionCurrShrink.Category := 'Project';
         FActionCurrShrink.Caption := 'Shrink Current File';
         FActionCurrShrink.Hint := 'Shrink Current File';
         FActionCurrShrink.Name := 'ShrinkCurrAction';
         FActionCurrShrink.Visible := True;
         FActionCurrShrink.OnExecute := CurrShrinkExecute;
         FActionCurrShrink.Enabled := True;

         FMenuCurrShrink := TMenuItem.Create(nil);
         FMenuCurrShrink.Name := 'ShrinkCurr';
         FMenuCurrShrink.Caption := 'Shrink Current File';
         FMenuCurrShrink.AutoHotkeys := maAutomatic;
         FMenuCurrShrink.Action := FActionCurrShrink;

         NTAServices.AddActionMenu(FProjectMenu.Name, FActionCurrShrink, FMenuCurrShrink, False, True);

         Bmp.LoadFromResourceName(HInstance, 'ShrinkBmp');

         {$IFDEF VER360}
             ImageIndex := AddGraphicToVirtualImageList(bmp, NTAServices.ImageList as TVirtualImageList, '', False);
         {$ELSEIF VER350}
             ImageIndex := AddGraphicToVirtualImageList(bmp, NTAServices.ImageList as TVirtualImageList, '', False);
         {$ELSE}
             ImageIndex := AddIconToImageList(AWizAction.FIcon, Svcs40.ImageList, False);
         {$ENDIF}

         FActionCurrShrink.ImageIndex := ImageIndex;
         FMenuCurrShrink.ImageIndex := ImageIndex;

         FActionCurrUnShrink := TAction.Create(nil);
         FActionCurrUnShrink.Category := 'Project';
         FActionCurrUnShrink.Caption := 'UnShrink Current File';
         FActionCurrUnShrink.Hint := 'UnShrink Current File';
         FActionCurrUnShrink.Name := 'UnShrinkCurrAction';
         FActionCurrUnShrink.Visible := True;
         FActionCurrUnShrink.OnExecute := CurrUnShrinkExecute;
         FActionCurrUnShrink.Enabled := True;

         FMenuCurrUnShrink := TMenuItem.Create(nil);
         FMenuCurrUnShrink.Name := 'UnShrinkCurr';
         FMenuCurrUnShrink.Caption := 'UnShrink Current File';
         FMenuCurrUnShrink.AutoHotkeys := maAutomatic;
         FMenuCurrUnShrink.Action := FActionCurrUnShrink;

         NTAServices.AddActionMenu(FProjectMenu.Name, FActionCurrUnShrink, FMenuCurrUnShrink, False, True);

         Bmp.LoadFromResourceName(HInstance, 'UnShrinkBmp');

         {$IFDEF VER360}
             ImageIndex := AddGraphicToVirtualImageList(bmp, NTAServices.ImageList as TVirtualImageList, '', False);
         {$ELSEIF VER350}
             ImageIndex := AddGraphicToVirtualImageList(bmp, NTAServices.ImageList as TVirtualImageList, '', False);
         {$ELSE}
             ImageIndex := AddIconToImageList(AWizAction.FIcon, Svcs40.ImageList, False);
         {$ENDIF}

         FActionCurrUnShrink.ImageIndex := ImageIndex;
         FMenuCurrUnShrink.ImageIndex := ImageIndex;

         FActionUnShrinkAll := TAction.Create(nil);
         FActionUnShrinkAll.Category := 'Project';
         FActionUnShrinkAll.Caption := 'UnShrink JNI Files';
         FActionUnShrinkAll.Hint := 'Project UnShrink JNI Files';
         FActionUnShrinkAll.Name := 'UnShrinkAllAction';
         FActionUnShrinkAll.Visible := True;
         FActionUnShrinkAll.OnExecute := UnShrinkAllExecute;
         FActionUnShrinkAll.Enabled := True;

         FMenuUnShrinkAll := TMenuItem.Create(nil);
         FMenuUnShrinkAll.Name := 'UnShrinkAll';
         FMenuUnShrinkAll.Caption := 'UnShrink JNI Files';
         FMenuUnShrinkAll.AutoHotkeys := maAutomatic;
         FMenuUnShrinkAll.Action := FActionUnShrinkAll;

         NTAServices.AddActionMenu(FProjectMenu.Name, FActionUnShrinkAll, FMenuUnShrinkAll, False, True);

         Bmp.LoadFromResourceName(HInstance, 'UnShrinkAllBmp');

         {$IFDEF VER360}
             ImageIndex := AddGraphicToVirtualImageList(bmp, NTAServices.ImageList as TVirtualImageList, '', False);
         {$ELSEIF VER350}
             ImageIndex := AddGraphicToVirtualImageList(bmp, NTAServices.ImageList as TVirtualImageList, '', False);
         {$ELSE}
             ImageIndex := AddIconToImageList(AWizAction.FIcon, Svcs40.ImageList, False);
         {$ENDIF}

         FActionUnShrinkAll.ImageIndex := ImageIndex;
         FMenuUnShrinkAll.ImageIndex := ImageIndex;

         Bmp.Free;

         FWait := TFWait.Create(nil);
         TBADIToolsAPIFunctions.RegisterFormClassForTheming(TFWait, FWait);

      end;

end;

procedure TJavaLoginMethodsExpert.RemoveActionFromToolbar(AAction: TAction);
var
  Services : INTAServices;
begin
  Services := (BorlandIDEServices as INTAServices);

  RemoveAction(AAction, Services.ToolBar[sCustomToolBar]);
  RemoveAction(AAction, Services.ToolBar[sDesktopToolBar]);
  RemoveAction(AAction, Services.ToolBar[sStandardToolBar]);
  RemoveAction(AAction, Services.ToolBar[sDebugToolBar]);
  RemoveAction(AAction, Services.ToolBar[sViewToolBar]);
//  RemoveAction(AAction, Services.ToolBar['InternetToolBar']);
end;

procedure TJavaLoginMethodsExpert.RemoveAction(AAction: TAction; AToolbar: TToolbar);
var
  iCounter: Integer;
  btnTool : TToolButton;
begin
  for iCounter := AToolbar.ButtonCount - 1 downto 0 do
  begin
    btnTool := AToolbar.Buttons[iCounter];
    if btnTool.Action = AAction then
    begin
      AToolbar.Perform(CM_CONTROLCHANGE, WParam(btnTool), 0);
      btnTool.Free;
    end;
  end;
end;

function TJavaLoginMethodsExpert.AddAction(ACaption, AHint, AName: String;
  AExecuteEvent, AUpdateEvent: TNotifyEvent): TAction;
var
  Service : INTAServices;
begin
  Service := (BorlandIDEServices as INTAServices);

  Result := TAction.Create(Service.ActionList);
  with Result do
  begin
    ActionList := Service.ActionList;
    Category := 'Build';
    Caption := ACaption;
    Hint := AHint;
    Name := AName;
    Visible := True;
    OnExecute := AExecuteEvent;
    OnUpdate := AUpdateEvent;
  end;
end;

destructor TJavaLoginMethodsExpert.Destroy;
begin

   FMenuShrinkAll.Free;
   FActionShrinkAll.Free;
   FMenuCurrShrink.Free;
   FActionCurrShrink.Free;
   FMenuUnShrinkAll.Free;
   FActionUnShrinkAll.Free;
   FMenuCurrUnShrink.Free;
   FActionCurrUnShrink.Free;
   FWait.Free;

   inherited Destroy;
end;

class procedure TBADIToolsAPIFunctions.RegisterFormClassForTheming(
  const AFormClass: TCustomFormClass; const Component: TComponent);
begin

   {$IFDEF Ver320}
   Var
     ITS : IOTAIDEThemingServices250;
   {$ENDIF Ver320}
   {$IFDEF Ver330}
   Var
     ITS : IOTAIDEThemingServices250;
   {$ENDIF Ver330}
   {$IFDEF Ver340}
   Var
     ITS : IOTAIDEThemingServices;
  {$ENDIF Ver340}

   Begin

     {$IFDEF Ver340}
     If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) Then
       If ITS.IDEThemingEnabled Then
         Begin
           ITS.RegisterFormClass(AFormClass);
           If Assigned(Component) Then
             ITS.ApplyTheme(Component);
         End;
     {$ENDIF Ver340}

     {$IFDEF Ver330}
     If Supports(BorlandIDEServices, IOTAIDEThemingServices250, ITS) Then
       If ITS.IDEThemingEnabled Then
         Begin
           ITS.RegisterFormClass(AFormClass);
           If Assigned(Component) Then
             ITS.ApplyTheme(Component);
         End;
     {$ENDIF Ver330}
     {$IFDEF Ver320}
     If Supports(BorlandIDEServices, IOTAIDEThemingServices250, ITS) Then
       If ITS.IDEThemingEnabled Then
         Begin
           ITS.RegisterFormClass(AFormClass);
           If Assigned(Component) Then
             ITS.ApplyTheme(Component);
         End;
     {$ENDIF Ver320}
   End;

end;

initialization
  FJavaLogInMethodsExpert := TJavaLoginMethodsExpert.Instance;
finalization
  FreeAndNil(FJavaLogInMethodsExpert);
end.
