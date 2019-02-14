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

unit ClientExchangeFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xml_doc;

type

  { TSenderExchangeInformationEx }

  TSenderExchangeInformationEx = class(TXmlSerializationObject) //%Таблица 7.3
  private
    FFullName: string;
    FIdentifierSenderOperator: string;
    FInn: string;
    procedure SetFullName(AValue: string);
    procedure SetIdentifierSenderOperator(AValue: string);
    procedure SetInn(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
  public
  published
    property FullName:string read FFullName write SetFullName;
    property Inn:string read FInn write SetInn;
    property IdentifierSenderOperator:string read FIdentifierSenderOperator write SetIdentifierSenderOperator;
  end;

  { TParticipantsInformationEx }

  TParticipantsInformationEx = class(TXmlSerializationObject)   //%Таблица 7.2
  private
    FRecipientInfo: string;
    FSenderExchangeInformationEx: TSenderExchangeInformationEx;
    FSenderInfo: string;
    procedure SetRecipientInfo(AValue: string);
    procedure SetSenderInfo(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property SenderInfo:string read FSenderInfo write SetSenderInfo;
    property RecipientInfo:string read FRecipientInfo write SetRecipientInfo;
    property SenderExchangeInformationEx:TSenderExchangeInformationEx read FSenderExchangeInformationEx;
  end;

  { TClientExchangeFile }

  TClientExchangeFile = class(TAbstractExchangeFile)   //%Таблица 7.1
  private
    FParticipantsInformationEx: TParticipantsInformationEx;
    //FDocument: TInvoceDocument;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property ParticipantsInformationEx:TParticipantsInformationEx read FParticipantsInformationEx;
    //property Document:TInvoceDocument read FDocument;
  end;

implementation

{ TSenderExchangeInformationEx }

procedure TSenderExchangeInformationEx.SetFullName(AValue: string);
begin
  if FFullName=AValue then Exit;
  FFullName:=AValue;
  ModifiedProperty('FullName');
end;

procedure TSenderExchangeInformationEx.SetIdentifierSenderOperator(
  AValue: string);
begin
  if FIdentifierSenderOperator=AValue then Exit;
  FIdentifierSenderOperator:=AValue;
  ModifiedProperty('IdentifierSenderOperator');
end;

procedure TSenderExchangeInformationEx.SetInn(AValue: string);
begin
  if FInn=AValue then Exit;
  FInn:=AValue;
  ModifiedProperty('Inn');
end;

procedure TSenderExchangeInformationEx.InternalRegisterPropertys;
begin
  RegisterProperty('FullName', 'НаимОрг', 'О', 'Наименование', 1, 1000);
  RegisterProperty('Inn', 'ИННЮЛ', 'О', 'ИНН', 10, 10);
  RegisterProperty('IdentifierSenderOperator', 'ИдЭДО', 'О', 'Идентификатор оператора электронного документооборота отправителя файла обмена информации покупателя', 3, 3);
end;

{ TParticipantsInformationEx }

procedure TParticipantsInformationEx.InternalRegisterPropertys;
begin
  RegisterProperty('SenderInfo', 'ИдОтпр', 'О', 'Идентификатор участника документооборота - отправителя файла обмена информации покупателя', 4, 46);
  RegisterProperty('RecipientInfo', 'ИдПол', 'О', 'Идентификатор участника документооборота - получателя файла обмена информации покупателя', 4, 46);
  RegisterProperty('SenderExchangeInformationEx', 'СвОЭДОтпр', 'Н', 'Сведения об операторе электронного документооборота отправителя файла обмена информации покупателя', -1, -1);
end;

procedure TParticipantsInformationEx.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FSenderExchangeInformationEx:=TSenderExchangeInformationEx.Create;
end;

procedure TParticipantsInformationEx.SetRecipientInfo(AValue: string);
begin
  if FRecipientInfo=AValue then Exit;
  FRecipientInfo:=AValue;
end;

procedure TParticipantsInformationEx.SetSenderInfo(AValue: string);
begin
  if FSenderInfo=AValue then Exit;
  FSenderInfo:=AValue;
end;

destructor TParticipantsInformationEx.Destroy;
begin
  FreeAndNil(FSenderExchangeInformationEx);
  inherited Destroy;
end;

{ TClientExchangeFile }

procedure TClientExchangeFile.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('ParticipantsInformationEx', 'СвУчДокОбор', 'О', 'Сведения об участниках электронного документооборота', -1, -1);
  (*
  Документ об отгрузке товаров (выполнении работ), передаче имущественных прав (документ об оказании услуг), включающий в себя счет-фактуру (информация покупателя), или документ об отгрузке товаров (выполнении работ), передаче имущественных прав (документ об оказании услуг) (информация покупателя) 	ИнфПок 	С 		О 	Состав элемента представлен в таблице 7.4
  *)
end;

procedure TClientExchangeFile.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FParticipantsInformationEx:=TParticipantsInformationEx.Create;
end;

destructor TClientExchangeFile.Destroy;
begin
  FreeAndNil(FParticipantsInformationEx);
  inherited Destroy;
end;

end.

