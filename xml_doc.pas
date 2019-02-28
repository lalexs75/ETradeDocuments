{ interface library for FPC and Lazarus

  Copyright (C) 2019 Lagunov Aleksey alexs75@yandex.ru

  Генерация xml файлов для электронного документооборота

  Структуры данных базируются на основании "Приказ ФНС РФ от 19.12.2018 N ММВ-7-15/820@"

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit xml_doc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM;

type
  EExchangeDefinitionError = class(Exception);


  { TPropertyDef }

  TPropertyDef = class
  private
    FRequaredAttribs:string;
    FCaption: string;
    FMaxSize: integer;
    FMinSize: integer;
    FModified: boolean;
    FPropertyName: string;
    FRequared: boolean;
    FSimpleObject: Boolean;
    FXMLName: string;
  public
    property PropertyName:string read FPropertyName;
    property Caption:string read FCaption;
    property XMLName:string read FXMLName;
    property Modified:boolean read FModified;
    property Requared:boolean read FRequared;
    property MinSize:integer read FMinSize;
    property MaxSize:integer read FMaxSize;
    property SimpleObject:Boolean read FSimpleObject;
  end;

  { TPropertyList }

  TPropertyList = class
  private
    FList:TFPList;
    function GetCount: integer;
    function GetItems(AIndex: integer): TPropertyDef;
    procedure ClearModified;
  public
    constructor Create;
    destructor Destroy; override;

    function PropertyByName(APropertyName:string):TPropertyDef;
    function PropertyByXMLName(AXMLName:string):TPropertyDef;
    function Add(const APropertyName, AXMLName, ARequaredAttribs, ACaption:string; AMinSize, AMaxSize:integer):TPropertyDef;
    procedure Clear;
    property Count:integer read GetCount;
    property Items[AIndex:integer]:TPropertyDef read GetItems; default;
  end;

  { TXmlSerializationObject }

  TXmlSerializationObject = class
  private
    FPropertyList:TPropertyList;
    procedure InternalRead(AElement: TDOMNode);
    procedure DoLoadAtributes(AElement: TDOMNode);
    procedure DoLoadChild(AElement: TDOMNode);

    procedure InternalWrite(FXML: TXMLDocument; AElement: TDOMElement);
    procedure InternalWriteChild(FXML: TXMLDocument; AChild:TObject; AElement: TDOMElement; P: TPropertyDef);
    procedure SetAtribute(P: TDOMElement; AttribName, AttribValue:DOMString; AMaxLen:Integer);
    function CreateElement(FXML: TXMLDocument; AParent:TDOMNode; AName:string):TDOMElement;
    procedure WriteXMLWin1251(Element: TDOMNode; const AFileName: String); overload;
  protected
    procedure RegisterProperty(APropertyName, AXMLName, ARequaredAttribs, ACaption:string; AMinSize, AMaxSize:integer);
    procedure ModifiedProperty(APropertyName:string);
    procedure InternalRegisterPropertys; virtual; abstract;
    procedure InternalInitChilds; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveToXML(AFileName:string);
    procedure LoadFromXML(AFileName:string);
  end;

  TXmlSerializationObjectClass = class of TXmlSerializationObject;

  { TXmlSerializationObjectList }

  TXmlSerializationObjectList = class
  private
    FList:TFPList;
    FBaseClass:TXmlSerializationObjectClass;
    function GetCount: Integer;
  protected
    function InternalAddObject:TXmlSerializationObject;
    function InternalGetItem(AIndex: Integer):TXmlSerializationObject;
  public
    constructor Create(ABaseClass:TXmlSerializationObjectClass);
    procedure Clear;
    destructor Destroy; override;
    property Count:Integer read GetCount;
  end;

  { TAbstractExchangeFile }

  TAbstractExchangeFile = class(TXmlSerializationObject)
  private
    FApplicationVersion: string;
    FFileID: string;
    FFormatVersion: string;
    procedure SetApplicationVersion(AValue: string);
    procedure SetFileID(AValue: string);
    procedure SetFormatVersion(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property FileID:string read FFileID write SetFileID;
    property FormatVersion:string read FFormatVersion write SetFormatVersion;
    property ApplicationVersion:string read FApplicationVersion write SetApplicationVersion;
  end;

implementation
uses XMLRead, XMLWrite, xmliconv, TypInfo, LazUTF8;

{ TAbstractExchangeFile }

procedure TAbstractExchangeFile.SetApplicationVersion(AValue: string);
begin
  if FApplicationVersion=AValue then Exit;
  FApplicationVersion:=AValue;
  ModifiedProperty('ApplicationVersion');
end;

procedure TAbstractExchangeFile.SetFileID(AValue: string);
begin
  if FFileID=AValue then Exit;
  FFileID:=AValue;
  ModifiedProperty('FileID');
end;

procedure TAbstractExchangeFile.SetFormatVersion(AValue: string);
begin
  if FFormatVersion=AValue then Exit;
  FFormatVersion:=AValue;
  ModifiedProperty('FormatVersion');
end;

procedure TAbstractExchangeFile.InternalRegisterPropertys;
begin
  RegisterProperty('FileID', 'ИдФайл', 'О', 'Идентификатор файла', 1, 255);
  RegisterProperty('FormatVersion', 'ВерсФорм', 'О', 'Версия формата', 1, 5);
  RegisterProperty('ApplicationVersion', 'ВерсПрог', 'О', 'Версия программы, с помощью которой сформирован файл', 1, 40);
end;

procedure TAbstractExchangeFile.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TAbstractExchangeFile.Destroy;
begin
  inherited Destroy;
end;

{ TXmlSerializationObjectList }

function TXmlSerializationObjectList.GetCount: Integer;
begin
  Result:=FList.Count;
end;

constructor TXmlSerializationObjectList.Create(
  ABaseClass: TXmlSerializationObjectClass);
begin
  inherited Create;
  FList:=TFPList.Create;
  FBaseClass:=ABaseClass;
end;

procedure TXmlSerializationObjectList.Clear;
var
  i: Integer;
begin
  for i:=0 to FList.Count-1 do
    TXmlSerializationObject(FList[i]).Free;
  FList.Clear;
end;

destructor TXmlSerializationObjectList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

function TXmlSerializationObjectList.InternalAddObject: TXmlSerializationObject;
begin
  Result:=FBaseClass.Create;
  FList.Add(Result);
end;

function TXmlSerializationObjectList.InternalGetItem(AIndex: Integer
  ): TXmlSerializationObject;
begin
  Result:=TXmlSerializationObject(FList[AIndex]);
end;

{ TPropertyList }

function TPropertyList.GetCount: integer;
begin
  Result:=FList.Count;
end;

function TPropertyList.GetItems(AIndex: integer): TPropertyDef;
begin
  Result:=TPropertyDef(FList[AIndex]);
end;

procedure TPropertyList.ClearModified;
var
  i: Integer;
begin
  for i:=0 to FList.Count-1 do
    TPropertyDef(FList[I]).FModified:=false;
end;

constructor TPropertyList.Create;
begin
  inherited Create;
  FList:=TFPList.Create;

end;

destructor TPropertyList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

function TPropertyList.PropertyByName(APropertyName: string): TPropertyDef;
var
  P: TPropertyDef;
  i: Integer;
begin
  Result:=nil;
  APropertyName:=UpperCase(APropertyName);
  for i:=0 to FList.Count-1 do
  begin
    P:=GetItems(I);
    if UpperCase(P.FPropertyName) = APropertyName then
      Exit(P);
  end;
end;

function TPropertyList.PropertyByXMLName(AXMLName: string): TPropertyDef;
var
  P: TPropertyDef;
  i: Integer;
begin
  Result:=nil;
  for i:=0 to FList.Count-1 do
  begin
    P:=GetItems(I);
    if P.FXMLName = AXMLName then
      Exit(P);
  end;
end;

function TPropertyList.Add(const APropertyName, AXMLName, ARequaredAttribs,
  ACaption: string; AMinSize, AMaxSize: integer): TPropertyDef;
begin
  Result:=TPropertyDef.Create;
  FList.Add(Result);
  Result.FRequaredAttribs:=ARequaredAttribs;
  Result.FPropertyName:=APropertyName;
  Result.FCaption:=ACaption;
  Result.FXMLName:=AXMLName;
  Result.FRequared:=UTF8Pos('О', ARequaredAttribs) > 0;
  Result.FMaxSize:=AMaxSize;
  Result.FMinSize:=AMinSize;
  Result.FSimpleObject:=UTF8Pos('П', ARequaredAttribs) > 0;
end;

procedure TPropertyList.Clear;
var
  i: Integer;
begin
  for i:=0 to FList.Count-1 do
    TPropertyDef(FList[I]).Free;
  FList.Clear;
end;

{ TXmlSerializationObject }

procedure TXmlSerializationObject.InternalRead(AElement: TDOMNode);
begin
  if not Assigned(AElement) then Exit;

  DoLoadAtributes(AElement);
  DoLoadChild(AElement);

  FPropertyList.ClearModified;
end;

procedure TXmlSerializationObject.InternalWrite(FXML: TXMLDocument;
  AElement: TDOMElement);
var
  i: Integer;
  P: TPropertyDef;
  FProp: PPropInfo;
begin
  for i:=0 to FPropertyList.Count-1 do
  begin
    P:=FPropertyList[i];

    FProp:=GetPropInfo(Self, P.FPropertyName);
    if not Assigned(FProp) then
      raise Exception.CreateFmt('Not fond property %s.%s(%s)', [ClassName, P.PropertyName, P.Caption]);
    case FProp^.PropType^.Kind of
      tkChar,
      tkAString,
      tkWString,
      tkSString,
      tkLString : if P.Modified then SetAtribute(AElement, P.XMLName, GetStrProp(Self, P.PropertyName), P.FMaxSize);
//                  SetAtribute(P: TDOMElement; AttribName, AttribValue:string; AMaxLen:Integer);
//      tkBool : SetOrdProp(Self, FProp, Ord(ABuf.ReadAsBoolean));
//      tkQWord : SetOrdProp(Self, FProp, Ord(ABuf.ReadAsQWord));
{      tkInt64 : SetInt64Prop(Self, FProp, StrToInt64(S2));
      tkInteger : SetOrdProp(Self, FProp, StrToInt(S2));}
      tkClass: InternalWriteChild(FXML, TObject(PtrInt( GetOrdProp(Self, FProp))), AElement, P);
    end;
  end;
end;

procedure TXmlSerializationObject.DoLoadAtributes(AElement: TDOMNode);
var
  i: Integer;
  A: TDOMNode;
  S1, S2:string;
  P: TPropertyDef;
  FProp: PPropInfo;
begin
  if not Assigned(AElement) then Exit;
  for i:=0 to AElement.Attributes.Length-1 do
  begin
    A:=AElement.Attributes.Item[I];
    S1:=A.NodeName;
    S2:=A.NodeValue;

    P:=FPropertyList.PropertyByXMLName(S1);

    if Assigned(P) then
    begin
      FProp:=GetPropInfo(Self, P.FPropertyName);
      if not Assigned(FProp) then
        raise Exception.CreateFmt('Not fond property %s.%s(%s)', [ClassName, P.PropertyName, P.Caption]);

      case FProp^.PropType^.Kind of
        tkChar,
        tkAString,
        tkWString,
        tkSString,
        tkLString : SetStrProp(Self, FProp, S2);
(*      tkBool : SetOrdProp(Self, FProp, Ord(ABuf.ReadAsBoolean));
        tkQWord : SetOrdProp(Self, FProp, Ord(ABuf.ReadAsQWord));
*)
        tkInt64 : SetInt64Prop(Self, FProp, StrToInt64(S2));
        tkInteger : SetOrdProp(Self, FProp, StrToInt(S2));
(*
        tkSet                       : SetSetProp(t,PropInfo,S);
        tkFloat                     : SetFloatProp(t,PropInfo, Value);}
        tkEnumeration : SetOrdProp(Self, FProp, Ord(ABuf.ReadAsInteger));
        tkDynArray:LoadBytes(FProp, P);
        *)
      else
        raise exception.CreateFmt('sProtoBufUnknowPropType %s', [P.FPropertyName]);
      end;
    end
    else
      raise exception.CreateFmt('Not found property in %s for data field %s', [ClassName, S1]);
  end;
end;

procedure TXmlSerializationObject.DoLoadChild(AElement: TDOMNode);
var
  i: Integer;
  FNode: TDOMNode;
  P: TPropertyDef;
  FProp: PPropInfo;
  K: TTypeKind;
  FInst: TObject;
  R: TXmlSerializationObject;
  S2: DOMString;
begin
  for i:=0 to AElement.ChildNodes.Count-1 do
  begin
    FNode:=AElement.ChildNodes.Item[I];

    P:=FPropertyList.PropertyByXMLName(FNode.NodeName);
    if Assigned(P) then
    begin
      FProp:=GetPropInfo(Self, P.FPropertyName); //Retreive property informations
      if not Assigned(FProp) then
        raise Exception.CreateFmt('sProtoBufNotFondProperty %s', [P.FPropertyName]);

      K:=FProp^.PropType^.Kind;
      if (P.FSimpleObject) and (K <> tkClass) then
      begin
        S2:=FNode.TextContent;
        case FProp^.PropType^.Kind of
          tkChar,
          tkAString,
          tkWString,
          tkSString,
          tkLString   : SetStrProp(Self, FProp, S2);
        else
          raise exception.CreateFmt('sProtoBufUnknowPropType %s', [P.FPropertyName]);
        end;
      end
      else
      begin
        if K <> tkClass then
          raise Exception.CreateFmt('Not is class property %s', [P.FPropertyName]);

        FInst := TObject(PtrInt( GetOrdProp(Self, FProp)));
        if not Assigned(FInst) then
          raise Exception.CreateFmt('Not inited %s', [P.FPropertyName]);

        if FInst is TXmlSerializationObject then
          TXmlSerializationObject(FInst).InternalRead(FNode)
        else
        if FInst is TXmlSerializationObjectList then
        begin
          R:=TXmlSerializationObjectList(FInst).InternalAddObject;
          R.InternalRead(FNode)
        end;
      end;
    end
    else
      raise exception.CreateFmt('Unknow class prop %s.%s', [ClassName, FNode.NodeName]);
  end;
end;

procedure TXmlSerializationObject.SetAtribute(P: TDOMElement; AttribName,
  AttribValue: DOMString; AMaxLen: Integer);
begin
  if (AMaxLen > 0) and (UTF8Length(AttribValue) > AMaxLen) then
    raise Exception.CreateFmt('Значение атрибута слишком велико (%s - %d)', [AttribValue, AMaxLen]);
  P.SetAttribute(AttribName, AttribValue);
end;

function TXmlSerializationObject.CreateElement(FXML: TXMLDocument;
  AParent: TDOMNode; AName: string): TDOMElement;
begin
  Result:=FXML.CreateElement(AName);
  if Assigned(AParent) then
    AParent.AppendChild(Result);
end;

procedure TXmlSerializationObject.InternalWriteChild(FXML: TXMLDocument;
  AChild: TObject; AElement: TDOMElement; P: TPropertyDef);
var
  E: TDOMElement;
  Itm: TXmlSerializationObject;
  i: Integer;
begin
  if not Assigned(AChild) then Exit;
  if AChild is TXmlSerializationObject then
  begin
    E:=CreateElement(FXML, AElement, P.XMLName);
    TXmlSerializationObject(AChild).InternalWrite(FXML, E);
  end
  else
  if AChild is TXmlSerializationObjectList then
  begin
    for i:=0 to TXmlSerializationObjectList(AChild).Count-1 do
    begin
      Itm:=TXmlSerializationObjectList(AChild).InternalGetItem(I);
      E:=CreateElement(FXML, AElement, P.XMLName);
      Itm.InternalWrite(FXML, E);
    end;
  end
  else
    raise Exception.CreateFmt('Unkonw object - %s', [AChild.ClassName]);
end;

procedure TXmlSerializationObject.WriteXMLWin1251(Element: TDOMNode;
  const AFileName: String);
var
  F, F1:TextFile;
  S, SL: String;
begin
  S:=GetTempFileName;
  WriteXML(Element, S);
  AssignFile(F, S);
  Reset(F);

  AssignFile(F1, AFileName);
  Rewrite(F1);
  TextRec(F1).CodePage:=1251;

  ReadLn(F, SL);
  SL:='<?xml version="1.0" encoding="windows-1251"?>';
  WriteLn(F1, SL);

  while not Eof(F) do
  begin
    ReadLn(F, SL);
    WriteLn(F1, SL)
  end;
  CloseFile(F);
  CloseFile(F1);
  DeleteFile(S);
end;

procedure TXmlSerializationObject.InternalInitChilds;
begin

end;

procedure TXmlSerializationObject.RegisterProperty(APropertyName, AXMLName,
  ARequaredAttribs, ACaption: string; AMinSize, AMaxSize: integer);
begin
  FPropertyList.Add(APropertyName, AXMLName, ARequaredAttribs, ACaption, AMinSize, AMaxSize);
end;

procedure TXmlSerializationObject.ModifiedProperty(APropertyName: string);
var
  P: TPropertyDef;
begin
  P:=FPropertyList.PropertyByName(APropertyName);
  if Assigned(P) then
    P.FModified:=true
  else
    raise Exception.CreateFmt('Property not found: %s', [APropertyName]);
end;

constructor TXmlSerializationObject.Create;
begin
  inherited Create;
  FPropertyList:=TPropertyList.Create;

  InternalInitChilds;
  InternalRegisterPropertys;
end;

destructor TXmlSerializationObject.Destroy;
begin
  FreeAndNil(FPropertyList);
  inherited Destroy;
end;

procedure TXmlSerializationObject.SaveToXML(AFileName: string);
var
  FXML: TXMLDocument;
  E: TDOMElement;
begin
  FXML:=TXMLDocument.Create;
  E:=CreateElement(FXML, FXML, 'Файл');
  InternalWrite(FXML, E);
  WriteXMLWin1251(FXML, AFileName);
  //FreeAndNil(FXML);
  FXML.Free;
end;

procedure TXmlSerializationObject.LoadFromXML(AFileName: string);
var
  FXML: TXMLDocument;
begin
  ReadXMLFile(FXML, AFileName);
  InternalRead(FXML.DocumentElement);
  //FreeAndNil(FXML);
  FXML.Free;
end;

end.

