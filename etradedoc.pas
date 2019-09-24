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

unit ETradeDoc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, EAbstractDoc, InvoceExchangeFile,
  InvoceExchangeFile_5_02, ClientExchangeFile, DOM;

type
  TInvoceVersion = (ivNONE,  iv5_01, iv5_02);

  { TETradeDoc }

  TETradeDoc = class(TEAbstractDoc)
  private
    FInovoce5_01: TExchangeFile;
    FInovoce5_02: TExchangeFile5_02;
    FInvoceVersion: TInvoceVersion;
  protected
    function GetInvoiceVersion(const AXmlDoc:TXMLDocument):TInvoceVersion;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadInvoce(AFileName:string);
    procedure SaveInvoce(AData:TExchangeFile; AFileName:string);
    function LoadClientExchangeFile(AFileName:string):TClientExchangeFile;
    procedure Clear;

    property Inovoce5_01:TExchangeFile read FInovoce5_01;
    property Inovoce5_02:TExchangeFile5_02 read FInovoce5_02;
  published
    property InvoceVersion:TInvoceVersion read FInvoceVersion default iv5_01;
  end;

implementation
uses XMLRead;

{ TETradeDoc }

procedure TETradeDoc.Clear;
begin
  if Assigned(FInovoce5_01) then
    FreeAndNil(FInovoce5_01);
  if Assigned(FInovoce5_02) then
    FreeAndNil(FInovoce5_02);
  FInvoceVersion:=ivNONE;
end;

function TETradeDoc.GetInvoiceVersion(const AXmlDoc: TXMLDocument
  ): TInvoceVersion;
function NodeValue(ANode:TDOMNode):String;
begin
  Result:=ANode.NodeValue;
end;

var
  P: Int64;
  S: String;
  RFile, RDoc, EVerF, EKnd, EFunc: TDOMNode;
  S1, S2, S3: String;
begin
  Result:=ivNONE;
  RFile:=nil;
  RDoc:=nil;
  EVerF:=nil;
  EKnd:=nil;
  EFunc:=nil;

  try
    if Assigned(AXmlDoc) then
    begin
      S:='Файл';
      RFile:=AXmlDoc.FindNode(S);
      S:='Документ';
      if Assigned(RFile) then
        RDoc:=RFile.FindNode(S);

      if Assigned(RFile) and Assigned(RDoc) then
      begin
        S:='ВерсФорм'; //="5.02"
        EVerF:=RFile.Attributes.GetNamedItem(S);
        //КНД="1115125" Функция="СЧФДОП"
        S:='КНД';
        EKnd:=RDoc.Attributes.GetNamedItem(S);
        S:='Функция';
        EFunc:=RDoc.Attributes.GetNamedItem(S);
        if Assigned(EVerF) and Assigned(EKnd) and Assigned(EFunc) then
        begin
          S1:=EVerF.NodeValue;
          S2:=EKnd.NodeValue;
          //S3:=EFunc.NodeValue;
          S3:=NodeValue(EFunc);

          if S3 = 'СЧФ' then
          begin
            if S1 = '5.02' then
              Result:=iv5_02
            else
            if S1 = '5.01' then
              Result:=iv5_01
          end
          else
          if S3 = 'СЧФДОП' then
          begin
            if S1 = '5.02' then
              Result:=iv5_02
            else
            if S1 = '5.01' then
              Result:=iv5_01
          end;
        end;
      end;
          //invoice_05_01_01
          //invoice_05_01_03
          //invoice_05_02_01
          //utd_05_01_01
          //utd_05_01_02
          //utd_05_01_04
          //utd_05_01_05
          //utd_05_02_01
          //utd820_05_01_01
    end;
  finally
  end;
end;

constructor TETradeDoc.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInvoceVersion:=iv5_01;
  FPrfix:='ON_NSCHFDOPPR';
end;

destructor TETradeDoc.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TETradeDoc.LoadInvoce(AFileName: string);
var
  FXML: TXMLDocument;
  R: TInvoceVersion;
begin
  Clear;
  ReadXMLFile(FXML, AFileName);
  R:=GetInvoiceVersion(FXML);
  case R of
    iv5_01:begin
      FInovoce5_01:=TExchangeFile.Create;
      FInovoce5_01.LoadFromXML(FXML);
    end;
    iv5_02:begin
      FInovoce5_02:=TExchangeFile5_02.Create;
      FInovoce5_02.LoadFromXML(FXML);
    end;
  end;
  FXML.Free;
end;

procedure TETradeDoc.SaveInvoce(AData: TExchangeFile; AFileName: string);
begin
  AData.SaveToFile(AFileName);
end;

function TETradeDoc.LoadClientExchangeFile(AFileName: string
  ): TClientExchangeFile;
begin
  Result:=TClientExchangeFile.Create;
  Result.LoadFromFile(AFileName);
end;

end.

