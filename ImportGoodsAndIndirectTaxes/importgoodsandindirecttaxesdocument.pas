{ interface library for FPC and Lazarus

  Copyright (C) 2019 Lagunov Aleksey alexs75@yandex.ru

  Генерация xml файлов для электронного документооборота
  Формат заявления о ввозе товаров и уплате косвенных налогов российского налогоплательщика

  Структуры данных базируются на основании "Приказ от 19.11.2014 № ММВ-7-6/590@"

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

unit ImportGoodsAndIndirectTaxesDocument;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xml_doc;

type

  { TLegalEntityInformation }

  TLegalEntityInformation = class(TXmlSerializationObject) //%Таблица 4.16
  private
    FFullName: string;
    FINN: string;
    FKPP: string;
    procedure SetFullName(AValue: string);
    procedure SetINN(AValue: string);
    procedure SetKPP(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property FullName:string read FFullName write SetFullName;
    property INN:string read FINN write SetINN;
    property KPP:string read FKPP write SetKPP;
  end;


  { TSenderInfo }

  TSenderInfo = class(TXmlSerializationObject) //%Таблица 4.3
  private
    FLegalEntityInformation: TLegalEntityInformation;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property LegalEntityInformation:TLegalEntityInformation read FLegalEntityInformation;
    //Отправитель - физическое лицо
  end;

  { TImportGoodsAndIndirectTaxesDocument }

  TImportGoodsAndIndirectTaxesDocument = class(TXmlSerializationObject) //%Таблица 4.2
  private
    FDocumentDate: string;
    FKND: string;
    FSenderInfo: TSenderInfo;
    procedure SetDocumentDate(AValue: string);
    procedure SetKND(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property KND:string read FKND write SetKND;
    property DocumentDate:string read FDocumentDate write SetDocumentDate;
    property SenderInfo:TSenderInfo read FSenderInfo;
//    property SignerInfo:TSignerInfo read FSignerInfo;
//    property DeclarationInfo:TDeclarationInfo read FDeclarationInfo;
//    property Contracts:TContracts read FContracts;
(*
Отправитель – организация
Отправитель - физическое лицо
*)
  end;

implementation

{ TLegalEntityInformation }

procedure TLegalEntityInformation.SetFullName(AValue: string);
begin
  if FFullName=AValue then Exit;
  FFullName:=AValue;
  ModifiedProperty('FullName');
end;

procedure TLegalEntityInformation.SetINN(AValue: string);
begin
  if FINN=AValue then Exit;
  FINN:=AValue;
  ModifiedProperty('INN');
end;

procedure TLegalEntityInformation.SetKPP(AValue: string);
begin
  if FKPP=AValue then Exit;
  FKPP:=AValue;
  ModifiedProperty('KPP');
end;

procedure TLegalEntityInformation.InternalRegisterPropertys;
begin
  RegisterProperty('FullName', 'НаимОрг', 'О', 'Наименование организации', 1, 400);
  RegisterProperty('INN', 'ИННЮЛ', 'О', 'ИНН организации', 10, 10);
  RegisterProperty('KPP', 'КПП', 'О', 'КПП', 9, 9);
end;

procedure TLegalEntityInformation.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TLegalEntityInformation.Destroy;
begin
  inherited Destroy;
end;

{ TSenderInfo }

procedure TSenderInfo.InternalRegisterPropertys;
begin

end;

procedure TSenderInfo.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FLegalEntityInformation:=TLegalEntityInformation.Create;
end;

destructor TSenderInfo.Destroy;
begin
  FreeAndNil(FLegalEntityInformation);
  inherited Destroy;
end;

{ TImportGoodsAndIndirectTaxesDocument }

procedure TImportGoodsAndIndirectTaxesDocument.SetDocumentDate(AValue: string);
begin
  if FDocumentDate=AValue then Exit;
  FDocumentDate:=AValue;
end;

procedure TImportGoodsAndIndirectTaxesDocument.SetKND(AValue: string);
begin
  if FKND=AValue then Exit;
  FKND:=AValue;
end;

procedure TImportGoodsAndIndirectTaxesDocument.InternalRegisterPropertys;
begin

end;

procedure TImportGoodsAndIndirectTaxesDocument.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FSenderInfo:=TSenderInfo.Create;
end;

destructor TImportGoodsAndIndirectTaxesDocument.Destroy;
begin
  FreeAndNil(FSenderInfo);
  inherited Destroy;
end;

end.

