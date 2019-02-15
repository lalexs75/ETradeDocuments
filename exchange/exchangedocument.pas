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
  Classes, SysUtils, xml_doc;

type

  { TExchangeDocument }

  TExchangeDocument = class(TXmlSerializationObject)   //%Таблица 7.4
  private
    FDateCreate: string;
    FDocumentCreator: string;
    FDocumentCreatorBase: string;
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
    (*
    Идентификация файла обмена счета-фактуры (информации продавца) или файла обмена информации продавца 	ИдИнфПрод 	С 		О 	Состав элемента представлен в таблице 7.5

    Содержание факта хозяйственной жизни 4 - сведения о принятии товаров (результатов выполненных работ), имущественных прав (о подтверждении факта оказания услуг) 	СодФХЖ4 	С 		О 	Состав элемента представлен в таблице 7.6

    Информация покупателя об обстоятельствах закупок для государственных и муниципальных нужд (для учета Федеральным казначейством денежных обязательств) 	ИнфПокГосЗакКазн 	С 		Н 	Состав элемента представлен в таблице 7.16.
    Обязателен при осуществлении закупок для обеспечения государственных и муниципальных нужд и (или) для формирования сведений о денежном обязательстве Федеральным казначейством

    Сведения о лице, подписывающем файл обмена информации покупателя в электронной форме 	Подписант 	С 		ОМ 	Состав элемента представлен в таблице 7.18.
    Фамилия, имя, отчество и другие сведения о лице указаны в элементе Подписант
    *)
  end;

implementation

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
  //RegisterProperty('', '', '', '', -1, -1);
  (*
  Идентификация файла обмена счета-фактуры (информации продавца) или файла обмена информации продавца 	ИдИнфПрод 	С 		О 	Состав элемента представлен в таблице 7.5

  Содержание факта хозяйственной жизни 4 - сведения о принятии товаров (результатов выполненных работ), имущественных прав (о подтверждении факта оказания услуг) 	СодФХЖ4 	С 		О 	Состав элемента представлен в таблице 7.6

  Информация покупателя об обстоятельствах закупок для государственных и муниципальных нужд (для учета Федеральным казначейством денежных обязательств) 	ИнфПокГосЗакКазн 	С 		Н 	Состав элемента представлен в таблице 7.16.
  Обязателен при осуществлении закупок для обеспечения государственных и муниципальных нужд и (или) для формирования сведений о денежном обязательстве Федеральным казначейством

  Сведения о лице, подписывающем файл обмена информации покупателя в электронной форме 	Подписант 	С 		ОМ 	Состав элемента представлен в таблице 7.18.
  Фамилия, имя, отчество и другие сведения о лице указаны в элементе Подписант
  *)
end;

procedure TExchangeDocument.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TExchangeDocument.Destroy;
begin
  inherited Destroy;
end;

end.

