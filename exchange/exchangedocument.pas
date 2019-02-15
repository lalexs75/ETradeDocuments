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

unit ExchangeDocument;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xml_doc, ExchangeInformation;

type

  { TExchangeFileIdentificatorSeller }

  TExchangeFileIdentificatorSeller = class(TXmlSerializationObject)   //%Таблица 7.5
  private
    FDateCreate: string;
    FFileName: string;
    FSign: string;
    FTimeCreate: string;
    procedure SetDateCreate(AValue: string);
    procedure SetFileName(AValue: string);
    procedure SetSign(AValue: string);
    procedure SetTimeCreate(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property FileName:string read FFileName write SetFileName;
    property DateCreate:string read FDateCreate write SetDateCreate;
    property TimeCreate:string read FTimeCreate write SetTimeCreate;
    property Sign:string read FSign write SetSign;
  end;

  { TExchangeDocument }

  TExchangeDocument = class(TXmlSerializationObject)   //%Таблица 7.4
  private
    FDateCreate: string;
    FDocumentCreator: string;
    FDocumentCreatorBase: string;
    FExchangeFileIdentificatorSeller: TExchangeFileIdentificatorSeller;
    FExchangeInformation: TExchangeInformation;
    FKND: string;
    FTimeCreate: string;
    procedure SetDateCreate(AValue: string);
    procedure SetDocumentCreator(AValue: string);
    procedure SetDocumentCreatorBase(AValue: string);
    procedure SetKND(AValue: string);
    procedure SetTimeCreate(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property KND:string read FKND write SetKND;
    property DateCreate:string read FDateCreate write SetDateCreate;
    property TimeCreate:string read FTimeCreate write SetTimeCreate;
    property DocumentCreator:string read FDocumentCreator write SetDocumentCreator;
    property DocumentCreatorBase:string read FDocumentCreatorBase write SetDocumentCreatorBase;
    property ExchangeFileIdentificatorSeller:TExchangeFileIdentificatorSeller read FExchangeFileIdentificatorSeller;
    property ExchangeInformation:TExchangeInformation read FExchangeInformation;
    (*
    Содержание факта хозяйственной жизни 4 - сведения о принятии товаров (результатов выполненных работ), имущественных прав (о подтверждении факта оказания услуг) 	СодФХЖ4 	С 		О
    Состав элемента представлен в таблице 7.6

    Информация покупателя об обстоятельствах закупок для государственных и муниципальных нужд (для учета Федеральным казначейством денежных обязательств) 	ИнфПокГосЗакКазн 	С 		Н
    Состав элемента представлен в таблице 7.16.
    Обязателен при осуществлении закупок для обеспечения государственных и муниципальных нужд и (или) для формирования сведений о денежном обязательстве Федеральным казначейством

    Сведения о лице, подписывающем файл обмена информации покупателя в электронной форме 	Подписант 	С 		ОМ
    Состав элемента представлен в таблице 7.18.
    Фамилия, имя, отчество и другие сведения о лице указаны в элементе Подписант
    *)
  end;

implementation

{ TExchangeFileIdentificatorSeller }

procedure TExchangeFileIdentificatorSeller.SetDateCreate(AValue: string);
begin
  if FDateCreate=AValue then Exit;
  FDateCreate:=AValue;
  ModifiedProperty('DateCreate');
end;

procedure TExchangeFileIdentificatorSeller.SetFileName(AValue: string);
begin
  if FFileName=AValue then Exit;
  FFileName:=AValue;
  ModifiedProperty('FileName');
end;

procedure TExchangeFileIdentificatorSeller.SetSign(AValue: string);
begin
  if FSign=AValue then Exit;
  FSign:=AValue;
  ModifiedProperty('Sign');
end;

procedure TExchangeFileIdentificatorSeller.SetTimeCreate(AValue: string);
begin
  if FTimeCreate=AValue then Exit;
  FTimeCreate:=AValue;
  ModifiedProperty('TimeCreate');
end;

procedure TExchangeFileIdentificatorSeller.InternalRegisterPropertys;
begin
  RegisterProperty('FileName', 'ИдФайлИнфПр', 'О', 'Идентификатор файла обмена информации продавца', 1, 255);
  RegisterProperty('DateCreate', 'ДатаФайлИнфПр', 'О', 'Дата формирования файла обмена информации продавца', 10, 10);
  RegisterProperty('TimeCreate', 'ВремФайлИнфПр', 'О', 'Время формирования файла обмена информации продавца', 8, 8);
  RegisterProperty('Sign', 'ЭП', 'ОМП', 'Электронная подпись файла обмена информации продавца', -1, -1);
end;

procedure TExchangeFileIdentificatorSeller.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TExchangeFileIdentificatorSeller.Destroy;
begin
  inherited Destroy;
end;

{ TExchangeDocument }

procedure TExchangeDocument.SetDateCreate(AValue: string);
begin
  if FDateCreate=AValue then Exit;
  FDateCreate:=AValue;
  ModifiedProperty('DateCreate');
end;

procedure TExchangeDocument.SetDocumentCreator(AValue: string);
begin
  if FDocumentCreator=AValue then Exit;
  FDocumentCreator:=AValue;
  ModifiedProperty('DocumentCreator');
end;

procedure TExchangeDocument.SetDocumentCreatorBase(AValue: string);
begin
  if FDocumentCreatorBase=AValue then Exit;
  FDocumentCreatorBase:=AValue;
  ModifiedProperty('DocumentCreatorBase');
end;

procedure TExchangeDocument.SetKND(AValue: string);
begin
  if FKND=AValue then Exit;
  FKND:=AValue;
  ModifiedProperty('KND');
end;

procedure TExchangeDocument.SetTimeCreate(AValue: string);
begin
  if FTimeCreate=AValue then Exit;
  FTimeCreate:=AValue;
  ModifiedProperty('TimeCreate');
end;

procedure TExchangeDocument.InternalRegisterPropertys;
begin
  RegisterProperty('KND', 'КНД', 'ОК', 'Код документа по КНД', 7, 7);
  RegisterProperty('DateCreate', 'ДатаИнфПок', 'О', 'Дата формирования файла обмена информации покупателя', 10, 10);
  RegisterProperty('TimeCreate', 'ВремИнфПок', 'О', 'Время формирования файла обмена информации покупателя', 8, 8);
  RegisterProperty('DocumentCreator', 'НаимЭконСубСост', 'О', 'Наименование экономического субъекта - составителя файла обмена информации покупателя', 1, 1000);
  RegisterProperty('DocumentCreatorBase', 'ОснДоверОргСост', 'Н', 'Основание, по которому экономический субъект является составителем файла обмена информации покупателя', 1, 120);
  RegisterProperty('ExchangeFileIdentificatorSeller', 'ИдИнфПрод', 'О', 'Идентификация файла обмена счета-фактуры (информации продавца) или файла обмена информации продавца', -1, -1);
  RegisterProperty('ExchangeInformation', 'СодФХЖ4', 'О', 'Содержание факта хозяйственной жизни 4 - сведения о принятии товаров (результатов выполненных работ), имущественных прав (о подтверждении факта оказания услуг)', -1, -1);
  //RegisterProperty('', '', '', '', -1, -1);
  (*
  Информация покупателя об обстоятельствах закупок для государственных и муниципальных нужд (для учета Федеральным казначейством денежных обязательств) 	ИнфПокГосЗакКазн 	С 		Н 	Состав элемента представлен в таблице 7.16.
  Обязателен при осуществлении закупок для обеспечения государственных и муниципальных нужд и (или) для формирования сведений о денежном обязательстве Федеральным казначейством

  Сведения о лице, подписывающем файл обмена информации покупателя в электронной форме 	Подписант 	С 		ОМ 	Состав элемента представлен в таблице 7.18.
  Фамилия, имя, отчество и другие сведения о лице указаны в элементе Подписант
  *)
end;

procedure TExchangeDocument.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FExchangeFileIdentificatorSeller:=TExchangeFileIdentificatorSeller.Create;
  FExchangeInformation:=TExchangeInformation.Create;
end;

destructor TExchangeDocument.Destroy;
begin
  FreeAndNil(FExchangeFileIdentificatorSeller);
  FreeAndNil(FExchangeInformation);
  inherited Destroy;
end;

end.

