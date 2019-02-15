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
unit ExchangeInformation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xml_doc;
type

  { TOperationCode }

  TOperationCode = class(TXmlSerializationObject)   //%Таблица 7.8
  private
    FDocumentName: string;
    FItogCode: string;
    procedure SetDocumentName(AValue: string);
    procedure SetItogCode(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property ItogCode:string read FItogCode write SetItogCode;
    property DocumentName:string read FDocumentName write SetDocumentName;
    (*
    Наименование документа, оформляющего расхождения 	НаимДокРасх 	А 	T(1-255) 	Н

    Код вида документа о расхождениях 	ВидДокРасх 	А 	T(=1) 	НК 	Принимает значение:
    2 - документ о приемке с расхождениями |
    3 - документ о расхождениях

    Номер документа покупателя о расхождениях 	НомДокРасх 	А 	T(1-255) 	Н

    Дата документа о расхождениях 	ДатаДокРасх 	А 	T(=10) 	Н 	Типовой элемент <ДатаТип>.
    Дата в формате ДД.ММ.ГГГГ

    Идентификатор файла обмена документа о расхождениях, сформированного покупателем 	ИдФайлДокРасх 	А 	T(1-255) 	Н
    *)
  end;

  { TAcceptanceInformation }

  TAcceptanceInformation = class(TXmlSerializationObject)   //%Таблица 7.7
  private
    FAcceptanceDate: string;
    FOperationCode: TOperationCode;
    FOperationContent: string;
    procedure SetAcceptanceDate(AValue: string);
    procedure SetOperationContent(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property OperationContent:string read FOperationContent write SetOperationContent;
    property AcceptanceDate:string read FAcceptanceDate write SetAcceptanceDate;
    property OperationCode:TOperationCode read FOperationCode;
    (*
    Код содержания операции
    КодСодОпер 	С 		Н
    Состав элемента представлен в таблице 7.8.
    Обязателен при отсутствии СодОпер

    Сведения о лице, принявшем товары (груз)
    СвЛицПрин 	С 		Н
    Состав элемента представлен в таблице 7.9

    *)
  end;

  { TExchangeInformation }

  TExchangeInformation = class(TXmlSerializationObject)   //%Таблица 7.6
  private
    FAcceptanceInformation: TAcceptanceInformation;
    FDocumentDate: string;
    FDocumentFunction: string;
    FDocumentName: string;
    FDocumentNumber: string;
    FOperationCode: string;
    procedure SetDocumentDate(AValue: string);
    procedure SetDocumentFunction(AValue: string);
    procedure SetDocumentName(AValue: string);
    procedure SetDocumentNumber(AValue: string);
    procedure SetOperationCode(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property DocumentName:string read FDocumentName write SetDocumentName;
    property DocumentFunction:string read FDocumentFunction write SetDocumentFunction;
    property DocumentNumber:string read FDocumentNumber write SetDocumentNumber;
    property DocumentDate:string read FDocumentDate write SetDocumentDate;
    property OperationCode:string read FOperationCode write SetOperationCode;
    property AcceptanceInformation:TAcceptanceInformation read FAcceptanceInformation;
    (*
    Информационное поле факта хозяйственной жизни 4 	ИнфПолФХЖ4 	С 		Н 	Состав элемента представлен в таблице 7.14
    *)
  end;

implementation

{ TOperationCode }

procedure TOperationCode.SetDocumentName(AValue: string);
begin
  if FDocumentName=AValue then Exit;
  FDocumentName:=AValue;
end;

procedure TOperationCode.SetItogCode(AValue: string);
begin
  if FItogCode=AValue then Exit;
  FItogCode:=AValue;
end;

procedure TOperationCode.InternalRegisterPropertys;
begin
  (*
  Код, обозначающий итог приемки товара (работ, услуг, прав) 	КодИтога 	А 	T(=1) 	ОК 	Принимает значение:
  1 - товары (работы, услуги, права) приняты без расхождений (претензий) |
  2 - товары (работы, услуги, права) приняты с расхождениями (претензией) |
  3 - товары (работы, услуги, права) не приняты

  Наименование документа, оформляющего расхождения 	НаимДокРасх 	А 	T(1-255) 	Н
  Код вида документа о расхождениях 	ВидДокРасх 	А 	T(=1) 	НК 	Принимает значение:
  2 - документ о приемке с расхождениями |
  3 - документ о расхождениях

  Номер документа покупателя о расхождениях 	НомДокРасх 	А 	T(1-255) 	Н

  Дата документа о расхождениях 	ДатаДокРасх 	А 	T(=10) 	Н 	Типовой элемент <ДатаТип>.
  Дата в формате ДД.ММ.ГГГГ

  Идентификатор файла обмена документа о расхождениях, сформированного покупателем 	ИдФайлДокРасх 	А 	T(1-255) 	Н
  *)
end;

procedure TOperationCode.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TOperationCode.Destroy;
begin
  inherited Destroy;
end;

{ TAcceptanceInformation }

procedure TAcceptanceInformation.SetAcceptanceDate(AValue: string);
begin
  if FAcceptanceDate=AValue then Exit;
  FAcceptanceDate:=AValue;
  ModifiedProperty('AcceptanceDate');
end;

procedure TAcceptanceInformation.SetOperationContent(AValue: string);
begin
  if FOperationContent=AValue then Exit;
  FOperationContent:=AValue;
  ModifiedProperty('OperationContent');
end;

procedure TAcceptanceInformation.InternalRegisterPropertys;
begin
  RegisterProperty('OperationContent', 'СодОпер', 'Н', 'Содержание операции (текст)', 1, 255);
  RegisterProperty('AcceptanceDate', 'ДатаПрин', 'Н', 'Дата принятия товаров (результатов выполненных работ), имущественных прав (подтверждения факта оказания услуг)', 10, 10);
  RegisterProperty('OperationCode', 'КодСодОпер', 'Н', 'Код содержания операции', -1, -1);
  (*
  Сведения о лице, принявшем товары (груз)
  СвЛицПрин 	С 		Н
  Состав элемента представлен в таблице 7.9
  *)
end;

procedure TAcceptanceInformation.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FOperationCode:=TOperationCode.Create;
end;

destructor TAcceptanceInformation.Destroy;
begin
  FreeAndNil(FOperationCode);
  inherited Destroy;
end;

{ TExchangeInformation }

procedure TExchangeInformation.SetDocumentDate(AValue: string);
begin
  if FDocumentDate=AValue then Exit;
  FDocumentDate:=AValue;
  ModifiedProperty('DocumentDate');
end;

procedure TExchangeInformation.SetDocumentFunction(AValue: string);
begin
  if FDocumentFunction=AValue then Exit;
  FDocumentFunction:=AValue;
  ModifiedProperty('DocumentFunction');
end;

procedure TExchangeInformation.SetDocumentName(AValue: string);
begin
  if FDocumentName=AValue then Exit;
  FDocumentName:=AValue;
  ModifiedProperty('DocumentName');
end;

procedure TExchangeInformation.SetDocumentNumber(AValue: string);
begin
  if FDocumentNumber=AValue then Exit;
  FDocumentNumber:=AValue;
  ModifiedProperty('DocumentNumber');
end;

procedure TExchangeInformation.SetOperationCode(AValue: string);
begin
  if FOperationCode=AValue then Exit;
  FOperationCode:=AValue;
  ModifiedProperty('OperationCode');
end;

procedure TExchangeInformation.InternalRegisterPropertys;
begin
  RegisterProperty('DocumentName', 'НаимДокОпрПр', 'О', 'Наименование первичного документа, согласованное сторонами сделки', 1, 255);
  RegisterProperty('DocumentFunction', 'Функция', 'О', 'Функция', 1, 6);
  RegisterProperty('DocumentNumber', 'НомСчФИнфПр', 'Н', 'Номер счета-фактуры (информации продавца)', 1, 1000);
  RegisterProperty('DocumentDate', 'ДатаСчФИнфПр', 'О', 'Дата составления (выписки) счета-фактуры (информации продавца)', 1, 10);
  RegisterProperty('OperationCode', 'ВидОперации', 'Н', 'Вид операции', 1, 255);
  RegisterProperty('AcceptanceInformation', 'СвПрин', 'О', 'Сведения о принятии товаров (результатов выполненных работ), имущественных прав (о подтверждении факта оказания услуг)', -1, -1);
  (*
  Информационное поле факта хозяйственной жизни 4 	ИнфПолФХЖ4 	С 		Н 	Состав элемента представлен в таблице 7.14
  *)
end;

procedure TExchangeInformation.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FAcceptanceInformation:=TAcceptanceInformation.Create;
end;

destructor TExchangeInformation.Destroy;
begin
  FreeAndNil(FAcceptanceInformation);
  inherited Destroy;
end;

end.

