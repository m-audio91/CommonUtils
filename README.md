# CommonUtils
last Free Pascal version used to compile was 3.1.1 trunk r39236
last Lazarus version used to compile was 1.9 trunk r58287  
may not work on older release versions.  
all units in this repository are independent from outside. but may require each other. for example uTimeCodeEdit requires uModalEditor and uTimeCode.

## CommonFileUtils.pas
Common routines to work with files.

## CommonGUIUtils.pas
Common routines to work with GUI.

## CommonNumeralUtils.pas
Common routines to work with numbers.

## CommonStrUtils.pas
Common routines to work with strings.

## uListBoxUtils.pas
same as uListViewUtils below. but for (Check)ListBox.

## uListViewUtils.pas
Utility to work with the standard ListView. DragDrop sorting items, delete, save,
load, etc all supporting multi-select.
usage example;
```pascal
uses
..., uListViewUtils;

...

procedure TForm1.FormCreate(Sender: TObject);
begin
  ListView1.OnDragDrop := @ListViewUtils.DragDrop;
  ListView1.OnDragOver := @ListViewUtils.DragOver;
  ListView1.OnMouseDown := @ListViewUtils.MouseDown;
  ListView1.OnMouseMove := @ListViewUtils.MouseMove;
  {$ifdef LINUX}
    ListView1.DragMode := dmManual;
  {$endif};
end;
 
procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
  ListViewUtils.DeleteItems(ListView1);
end;
 
procedure TForm1.SpeedButton3Click(Sender: TObject);
begin
  ListViewUtils.MoveItemsUp(ListView1);
end;
 
...
```

## uMinimalList.pas
A minimal generic list with virtual methods. mainly for small list of records. see [uGenericSubtitleFile](https://github.com/m-audio91/CodecUtils/blob/master/ugenericsubtitlefile.pas) for an example.


## uModalEditor.pas
A dialog form to work with anything you like. it's ChildSizing is set. It only contains OK and Cancel buttons.  
just create your controls after you've called create constructor and set parent and owner of your newly created controls to TModalEditor instance.  
the layout is top to bottom so the creation order matters.  
also see uTimeCodeEdit for an example of a descendant class.  

## uNumEditFloat.pas  
A simple dialog form to edit a float number.  

## uTimeCode.pas
A TimeCode and TimeCode List parser.
it can parse a TimeCode string written in twelve forms like `HH:MM:SS.ms` or `M:S.ms` or `SS.ms` etc.
there is ability to initialize TTimeCode with different separators like HH:MM:SS;ms.
you can also control precision of the milliseconds part. like 1 or 10 etc.
an example from BitHesab console project:
```pascal
uses
..., uTimeSlice;

...

procedure TBitHesab.ParseParams;
var
  ...
  TC: TTimeCode;
begin
  ...

  if HasOption('d', 'duration') then
  begin
    s := GetOptionValue('d', 'duration');
    TC.ValueAsString := s;
    FDuration := Trunc(TC.ValueAsDouble);
    if FDuration = 0 then
      Fatal(rsWrongDur)
    else
    begin
    b1 := ForceInRange(FDuration, MinDur, MaxDur);
      if b1 then
        Warn(rsClipping + 'duration');
    end;
  end;
end;
```

## uTimeCodeEdit.pas
A dialog form to work with uTimeCode unit.
an example function from one private project is:

```pascal
uses
..., uTimeCodeEdit;

...

procedure TFFTimeCodeListFilter.ItemNew(Sender: TObject);
var
  tce: TTimeCodeEdit;
begin
  tce := TTimeCodeEdit.Create(nil);
  try
    if tce.ShowModal = mrOk then
      FList.Items.Add(tce.Value);
  finally
    tce.Free;
  end;
end;  
```

## uTimeCodeFormatDialog.pas  
A dialog form to define format for further TimeCode inputs. such as paste from clipboard, read from file etc.  
an usage example from [SubzBor](https://github.com/m-audio91/SubzBor) project:  
```pascal
procedure TSBMain.DefineUserInputsFormat;
var
  fd: TTimeCodeFormatDialog;
begin
  SBDatas.TaskDlg.Execute(Self.Handle); //this just shows some information text
  fd := TTimeCodeFormatDialog.Create(Self);
  try
    fd.Value.TimeCodeFormat := FFormatSettings; //optional, you can prepare it with some previously stored setting
    fd.ShowModal;
    if fd.ModalResult = mrOK then
      FFormatSettings := fd.Value.TimeCodeFormat;
  finally
    fd.Free;
  end;
  FInputsFormatDefined := True;
end;
```  

## uTimeSlice.pas
A TimeSlice and TimeSlice List parser based on uTimeCode unit.
Here `00:00:00.000-00:00:00.001` is considered a valid slice of the time (a TimeSlice).
with a start and an ending TimeCode. when (start >= 0) and (end > start).
an example function from one private project is:
```pascal
uses
..., uTimeSlice;

...

function TimeSliceListToFFTimeLine(const ATimeSliceList: String): String;
var
  sl: TStringList;
  ts: TTimeSlice;
  s: String;
  i: Integer;
begin
  Result := EmptyStr;
  sl := TStringList.Create;
  try
    sl.Text := ATimeSliceList;
    if sl.Count = 0 then Exit;
    Result := ':enable=';
    for i := 0 to sl.Count-1 do
    begin
      ts.ValueAsString := sl[i];
      if i < 1 then
        s := EmptyStr
      else
        s := '+';
      Result := Result+s+'between(t\,'+FloatRound(ts.Value.StartPos.ValueAsDouble,3).ToString;
      Result := Result+'\,'+FloatRound(ts.Value.EndPos.ValueAsDouble,3).ToString+')';
    end;
  finally
    sl.Free;
  end;
end;
```
which converts from:
```
00:00:20.000-00:00:40.001
00:01:20.000-00:01:40.001
```
to:
```
:enable=between(t\,20\,40.001)+between(t\,80\,100.001)
```

## uTimeSliceEdit.pas
A dialog form to work with uTimeCode and uTimeSlice units. without delay support.
usage is the same as uTimeSliceEditEx.pas below.

## uTimeSliceEditEx.pas
A dialog form to work with uTimeCode and uTimeSlice units.
an usage example from [SubzBor](https://github.com/m-audio91/SubzBor) project:

```pascal
uses
..., uTimeSliceEditEx;

...

procedure TSBMain.AddTimeSliceClick(Sender: TObject);
var
  tse: TTimeSliceEditEx;
begin
  tse := TTimeSliceEditEx.Create(Self);
  try
    tse.ShowModal;
    if tse.ModalResult = mrOk then
      TimeSlicesList.Items.Add(tse.Value);
  finally
    tse.Free;
  end;
end;

procedure TSBMain.EditTimeSliceClick(Sender: TObject);
var
  tse: TTimeSliceEditEx;
begin
  tse := TTimeSliceEditEx.Create(Self);
  try
    if TimeSlicesList.ItemIndex >= 0 then
    begin
      tse.Value := TimeSlicesList.Items[TimeSlicesList.ItemIndex];
      tse.ShowModal;
      if tse.ModalResult = mrOk then
        TimeSlicesList.Items[TimeSlicesList.ItemIndex] := tse.Value;
    end;
  finally
    tse.Free;
  end;
end; 
```

## uUrlLabel.pas
A simple TLabel which opens it's Caption in systems default browser on click.  
usage example:
```pascal
uses
..., uUrlLabel;

...

procedure TSBAbout.FormCreate(Sender: TObject);
var
  AUrl: TUrlLabel;
begin
  AUrl := TUrlLabel.Create(Self);
  with AUrl do
  begin
    Parent := ContactBox;
    Font.Color := clBlue;
    Caption := 'http://www.google.com';
    //set positioning and spacing properties here
  end;
end;  
```
An extended version that has a property called URL is also included. see [FaSubrip](https://github.com/m-audio91/FaSubrip/blob/master/umain.pas)'s FormCreate for example.

