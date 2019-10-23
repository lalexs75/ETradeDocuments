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

unit TreasuryInformation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlobject;

type

  { TMonetaryObligationInformation }

  TMonetaryObligationInformation = class(TXmlSerializationObject)   //%Таблица 7.17
  private
    FAvanceSum: string;
    FDestinationCode: string;
    FFundsType: string;
    FKBK: string;
    FObjectCode: string;
    FRowNumber: string;
    procedure SetAvanceSum(AValue: string);
    procedure SetDestinationCode(AValue: string);
    procedure SetFundsType(AValue: string);
    procedure SetKBK(AValue: string);
    procedure SetObjectCode(AValue: string);
    procedure SetRowNumber(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property RowNumber:string read FRowNumber write SetRowNumber;
    property ObjectCode:string read FObjectCode write SetObjectCode;
    property FundsType:string read FFundsType write SetFundsType;
    property KBK:string read FKBK write SetKBK;
    property DestinationCode:string read FDestinationCode write SetDestinationCode;
    property AvanceSum:string read FAvanceSum write SetAvanceSum;
  end;

  { TMonetaryObligationInformationList }

  TMonetaryObligationInformationList = class(TXmlSerializationObjectList) //%Таблица 7.17
  private
    function GetItem(AIndex: Integer): TMonetaryObligationInformation; inline;
  public
    constructor Create;
    function CreateChild:TMonetaryObligationInformation;
    property Item[AIndex:Integer]:TMonetaryObligationInformation read GetItem; default;
  end;

  { TTreasuryInformation }

  TTreasuryInformation = class(TXmlSerializationObject)   //%Таблица 7.16
  private
    FAccountNumber: string;
    FBudgetAccountNumebr: string;
    FFinancialName: string;
    FMaxPayDate: string;
    FMonetaryObligationInformation: TMonetaryObligationInformationList;
    FMonetaryObligationNumber: string;
    FOKTMOCode: string;
    FOKTMOCodeLocation: string;
    FPayOrder: string;
    FPayType: string;
    FPurchaseID: string;
    FReestrNumber: string;
    FTreasuryCode: string;
    FTreasuryName: string;
    procedure SetAccountNumber(AValue: string);
    procedure SetBudgetAccountNumebr(AValue: string);
    procedure SetFinancialName(AValue: string);
    procedure SetMaxPayDate(AValue: string);
    procedure SetMonetaryObligationNumber(AValue: string);
    procedure SetOKTMOCode(AValue: string);
    procedure SetOKTMOCodeLocation(AValue: string);
    procedure SetPayOrder(AValue: string);
    procedure SetPayType(AValue: string);
    procedure SetPurchaseID(AValue: string);
    procedure SetReestrNumber(AValue: string);
    procedure SetTreasuryCode(AValue: string);
    procedure SetTreasuryName(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property PurchaseID:string read FPurchaseID write SetPurchaseID;
    property AccountNumber:string read FAccountNumber write SetAccountNumber;
    property FinancialName:string read FFinancialName write SetFinancialName;
    property ReestrNumber:string read FReestrNumber write SetReestrNumber;
    property BudgetAccountNumebr:string read FBudgetAccountNumebr write SetBudgetAccountNumebr;
    property TreasuryCode:string read FTreasuryCode write SetTreasuryCode;
    property TreasuryName:string read FTreasuryName write SetTreasuryName;
    property OKTMOCode:string read FOKTMOCode write SetOKTMOCode;
    property OKTMOCodeLocation:string read FOKTMOCodeLocation write SetOKTMOCodeLocation;
    property MaxPayDate:string read FMaxPayDate write SetMaxPayDate;
    property MonetaryObligationNumber:string read FMonetaryObligationNumber write SetMonetaryObligationNumber;
    property PayOrder:string read FPayOrder write SetPayOrder;
    property PayType:string read FPayType write SetPayType;
    property MonetaryObligationInformation:TMonetaryObligationInformationList read FMonetaryObligationInformation;
  end;

implementation

{ TMonetaryObligationInformation }

procedure TMonetaryObligationInformation.SetAvanceSum(AValue: string);
begin
  if FAvanceSum=AValue then Exit;
  FAvanceSum:=AValue;
  ModifiedProperty('AvanceSum');
end;

procedure TMonetaryObligationInformation.SetDestinationCode(AValue: string);
begin
  if FDestinationCode=AValue then Exit;
  FDestinationCode:=AValue;
  ModifiedProperty('DestinationCode');
end;

procedure TMonetaryObligationInformation.SetFundsType(AValue: string);
begin
  if FFundsType=AValue then Exit;
  FFundsType:=AValue;
  ModifiedProperty('FundsType');
end;

procedure TMonetaryObligationInformation.SetKBK(AValue: string);
begin
  if FKBK=AValue then Exit;
  FKBK:=AValue;
  ModifiedProperty('KBK');
end;

procedure TMonetaryObligationInformation.SetObjectCode(AValue: string);
begin
  if FObjectCode=AValue then Exit;
  FObjectCode:=AValue;
  ModifiedProperty('ObjectCode');
end;

procedure TMonetaryObligationInformation.SetRowNumber(AValue: string);
begin
  if FRowNumber=AValue then Exit;
  FRowNumber:=AValue;
  ModifiedProperty('RowNumber');
end;

procedure TMonetaryObligationInformation.InternalRegisterPropertys;
begin
  RegisterProperty('RowNumber', 'НомСтр', 'О', 'Номер строки таблицы информации продавца', 6, 6);
  RegisterProperty('ObjectCode', 'КодОбъектФАИП', 'Н', 'Код объекта капитального строительства федеральной адресной инвестиционной программы/код мероприятия по информатизации', 1, 24);
  RegisterProperty('FundsType', 'ВидСредств', 'ОК', 'Вид средств', 1, 1);
  RegisterProperty('KBK', 'КодПокБюджКласс', 'О', 'Код по бюджетной классификации (покупатель)', 20, 20);
  RegisterProperty('DestinationCode', 'КодЦелиПокуп', 'Н', 'Код цели (покупатель)', 1, 20);
  RegisterProperty('AvanceSum', 'СумАванс', 'О', 'Сумма перечисленного аванса', 1, 19);
end;

procedure TMonetaryObligationInformation.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TMonetaryObligationInformation.Destroy;
begin
  inherited Destroy;
end;

{ TMonetaryObligationInformationList }

function TMonetaryObligationInformationList.GetItem(AIndex: Integer
  ): TMonetaryObligationInformation;
begin
  Result:=TMonetaryObligationInformation(InternalGetItem(AIndex));
end;

constructor TMonetaryObligationInformationList.Create;
begin
  inherited Create(TMonetaryObligationInformation)
end;

function TMonetaryObligationInformationList.CreateChild: TMonetaryObligationInformation;
begin
  Result:=InternalAddObject as TMonetaryObligationInformation;
end;

{ TTreasuryInformation }

procedure TTreasuryInformation.SetPurchaseID(AValue: string);
begin
  if FPurchaseID=AValue then Exit;
  FPurchaseID:=AValue;
  ModifiedProperty('PurchaseID');
end;

procedure TTreasuryInformation.SetAccountNumber(AValue: string);
begin
  if FAccountNumber=AValue then Exit;
  FAccountNumber:=AValue;
  ModifiedProperty('AccountNumber');
end;

procedure TTreasuryInformation.SetBudgetAccountNumebr(AValue: string);
begin
  if FBudgetAccountNumebr=AValue then Exit;
  FBudgetAccountNumebr:=AValue;
  ModifiedProperty('BudgetAccountNumebr');
end;

procedure TTreasuryInformation.SetFinancialName(AValue: string);
begin
  if FFinancialName=AValue then Exit;
  FFinancialName:=AValue;
  ModifiedProperty('FinancialName');
end;

procedure TTreasuryInformation.SetMaxPayDate(AValue: string);
begin
  if FMaxPayDate=AValue then Exit;
  FMaxPayDate:=AValue;
  ModifiedProperty('MaxPayDate');
end;

procedure TTreasuryInformation.SetMonetaryObligationNumber(AValue: string);
begin
  if FMonetaryObligationNumber=AValue then Exit;
  FMonetaryObligationNumber:=AValue;
  ModifiedProperty('MonetaryObligationNumber');
end;

procedure TTreasuryInformation.SetOKTMOCode(AValue: string);
begin
  if FOKTMOCode=AValue then Exit;
  FOKTMOCode:=AValue;
  ModifiedProperty('OKTMOCode');
end;

procedure TTreasuryInformation.SetOKTMOCodeLocation(AValue: string);
begin
  if FOKTMOCodeLocation=AValue then Exit;
  FOKTMOCodeLocation:=AValue;
  ModifiedProperty('OKTMOCodeLocation');
end;

procedure TTreasuryInformation.SetPayOrder(AValue: string);
begin
  if FPayOrder=AValue then Exit;
  FPayOrder:=AValue;
  ModifiedProperty('PayOrder');
end;

procedure TTreasuryInformation.SetPayType(AValue: string);
begin
  if FPayType=AValue then Exit;
  FPayType:=AValue;
  ModifiedProperty('PayType');
end;

procedure TTreasuryInformation.SetReestrNumber(AValue: string);
begin
  if FReestrNumber=AValue then Exit;
  FReestrNumber:=AValue;
  ModifiedProperty('ReestrNumber');
end;

procedure TTreasuryInformation.SetTreasuryCode(AValue: string);
begin
  if FTreasuryCode=AValue then Exit;
  FTreasuryCode:=AValue;
  ModifiedProperty('TreasuryCode');
end;

procedure TTreasuryInformation.SetTreasuryName(AValue: string);
begin
  if FTreasuryName=AValue then Exit;
  FTreasuryName:=AValue;
  ModifiedProperty('TreasuryName');
end;

procedure TTreasuryInformation.InternalRegisterPropertys;
begin
  RegisterProperty('PurchaseID', 'ИдКодЗак', 'Н', 'Идентификационный код закупки', 1, 36);
  RegisterProperty('AccountNumber', 'ЛицСчетПок', 'О', 'Номер лицевого счета покупателя', 11, 11);
  RegisterProperty('FinancialName', 'НаимФинОргПок', 'О', 'Наименование финансового органа покупателя', 1, 2000);
  RegisterProperty('ReestrNumber', 'НомРеестрЗапПок', 'О', 'Номер реестровой записи покупателя по Реестру участников бюджетного процесса, а также юридических лиц, не являющихся участниками бюджетного процесса', 8, 8);
  RegisterProperty('BudgetAccountNumebr', 'УчНомБюдОбязПок', 'Н', 'Учетный номер бюджетного обязательства покупателя', 16, 19);
  RegisterProperty('TreasuryCode', 'КодКазначПок', 'Н', 'Код территориального органа Федерального казначейства покупателя', 4, 4);
  RegisterProperty('TreasuryName', 'НаимКазначПок', 'Н', 'Наименование территориального органа Федерального казначейства покупателя', 1, 2000);
  RegisterProperty('OKTMOCode', 'ОКТМОПок', 'ОК', 'Код покупателя в Общероссийском классификаторе территорий муниципальных образований', 8, 11);
  RegisterProperty('OKTMOCodeLocation', 'ОКТМОМесПост', 'НК', 'Код места поставки в Общероссийском классификаторе территорий муниципальных образований', 8, 11);
  RegisterProperty('MaxPayDate', 'ДатаОплПред', 'Н', 'Предельная дата оплаты', 10, 10);
  RegisterProperty('MonetaryObligationNumber', 'УчНомДенОбяз', 'Н', 'Учетный номер денежного обязательства', 22, 22);
  RegisterProperty('PayOrder', 'ОчерПлат', 'Н', 'Очередность платежа', 1, 1);
  RegisterProperty('PayType', 'ВидПлат', 'НК', 'Вид платежа', 1, 1);
  RegisterProperty('MonetaryObligationInformation', 'ИнфСведДенОбяз', 'ОМ', 'Информация для сведений о денежном обязательстве', -1, -1);
end;

procedure TTreasuryInformation.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FMonetaryObligationInformation:=TMonetaryObligationInformationList.Create;
end;

destructor TTreasuryInformation.Destroy;
begin
  FreeAndNil(FMonetaryObligationInformation);
  inherited Destroy;
end;

end.

