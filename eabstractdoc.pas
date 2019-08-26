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

unit EAbstractDoc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TEAbstractDoc }

  TEAbstractDoc = class(TComponent)
  private
    FAppVersion: string;
    FFormatVersion: string;
    FReciverID: string;
    FSenderID: string;
    FUniqueID: string;
    function GetFileDate: TDateTime;
    procedure SetAppVersion(AValue: string);
    procedure SetFileDate(AValue: TDateTime);
    procedure SetFormatVersion(AValue: string);
    procedure SetKND(AValue: string);
    procedure SetReciverID(AValue: string);
    procedure SetSenderID(AValue: string);
    procedure SetUniqueID(AValue: string);
  protected
    FKND: string;
    FFileDate:TDateTime;
    FPrfix:string;
  public
    function GetFileName:string;
    constructor Create(AOwner: TComponent); override;
  published
    property FormatVersion:string read FFormatVersion write SetFormatVersion;
    property AppVersion:string read FAppVersion write SetAppVersion;
    property SenderID:string read FSenderID write SetSenderID;
    property ReciverID:string read FReciverID write SetReciverID;
    property FileDate:TDateTime read GetFileDate write SetFileDate;
    property UniqueID:string read FUniqueID write SetUniqueID;
    property KND:string read FKND write SetKND;
  end;

implementation

{ TEAbstractDoc }

procedure TEAbstractDoc.SetAppVersion(AValue: string);
begin
  if FAppVersion=AValue then Exit;
  FAppVersion:=AValue;
end;

function TEAbstractDoc.GetFileDate: TDateTime;
begin
  if FFileDate < 0 then
    Result:=Date
  else
    FFileDate:=-1;
end;

procedure TEAbstractDoc.SetFileDate(AValue: TDateTime);
begin
  if FFileDate = AValue then Exit;
  FFileDate:=AValue;
end;

procedure TEAbstractDoc.SetFormatVersion(AValue: string);
begin
  if FFormatVersion=AValue then Exit;
  FFormatVersion:=AValue;
end;

procedure TEAbstractDoc.SetKND(AValue: string);
begin
  if FKND=AValue then Exit;
  FKND:=AValue;
end;

procedure TEAbstractDoc.SetReciverID(AValue: string);
begin
  if FReciverID=AValue then Exit;
  FReciverID:=AValue;
end;

procedure TEAbstractDoc.SetSenderID(AValue: string);
begin
  if FSenderID=AValue then Exit;
  FSenderID:=AValue;
end;

procedure TEAbstractDoc.SetUniqueID(AValue: string);
begin
  if FUniqueID=AValue then Exit;
  FUniqueID:=AValue;
end;

function TEAbstractDoc.GetFileName: string;
var
  G: TGUID;
  S: String;
begin
  if UniqueID = '' then
  begin
    CreateGUID(G);
    S:=GUIDToString(G);
    UniqueID:=Copy(S, 2, Length(S) - 2);
  end;
          //R_Т          //A                //О
  Result:=FPrfix + '_' + FReciverID + '_' + FSenderID + '_' + UniqueID;
end;

constructor TEAbstractDoc.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFileDate:=-1;
end;

end.

