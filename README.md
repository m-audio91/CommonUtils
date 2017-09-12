# CommonUtils
last Free Pascal version used to compile was 3.1.1 trunk r37023  
last Lazarus version used to compile was 1.9 trunk r55735  
may not work on older release versions.

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
  tce := TTimeCodeEdit.CreateNew(nil); //notice *CreateNew* constructor.
  try
    if tce.ShowModal = mrOk then
      FList.Items.Add(tce.Value);
  finally
    tce.Free;
  end;
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
  tse := TTimeSliceEditEx.CreateNew(Self); //notice *CreateNew* constructor.
  try
    tse.ShowModal;
    if tse.ModalResult = mrOk then
      TimeSlicesList.Items.Add(tse.Value);
  finally
    tse.Free;
  end;
end; 
```
