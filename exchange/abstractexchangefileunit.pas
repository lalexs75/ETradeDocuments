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

unit AbstractExchangeFileUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, xmlobject;

type
  { TAbstractExchangeFile }

  TAbstractExchangeFile = class(TXmlSerializationObject)
  private
    FApplicationVersion: string;
    FFileID: string;
    FFormatVersion: string;
    procedure SetApplicationVersion(AValue: string);
    procedure SetFileID(AValue: string);
    procedure SetFormatVersion(AValue: string);
    procedure WriteXMLWin1251(Element: TDOMNode; const AFileName: String);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
    procedure SaveToFile(AFileName:string); override;
  published
    property FileID:string read FFileID write SetFileID;
    property FormatVersion:string read FFormatVersion write SetFormatVersion;
    property ApplicationVersion:string read FApplicationVersion write SetApplicationVersion;
  end;

implementation
uses XMLWrite;

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

procedure TAbstractExchangeFile.WriteXMLWin1251(Element: TDOMNode;
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

procedure TAbstractExchangeFile.InternalRegisterPropertys;
begin
  RegisterProperty('FileID', 'ИдФайл', [xsaRequared], 'Идентификатор файла', 1, 255);
  RegisterProperty('FormatVersion', 'ВерсФорм', [xsaRequared], 'Версия формата', 1, 5);
  RegisterProperty('ApplicationVersion', 'ВерсПрог', [xsaRequared], 'Версия программы, с помощью которой сформирован файл', 1, 40);
end;

procedure TAbstractExchangeFile.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TAbstractExchangeFile.Destroy;
begin
  inherited Destroy;
end;

procedure TAbstractExchangeFile.SaveToFile(AFileName: string);
begin
  inherited SaveToFile(AFileName);
end;

end.

