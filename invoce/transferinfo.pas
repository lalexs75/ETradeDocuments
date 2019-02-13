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
unit TransferInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xml_doc, OrganizationInfo, AdditionalInfo;

type

  { TShipmentBase }

  TShipmentBase = class(TXmlSerializationObject) //%Таблица 5.48
  private
    FBaseDocumentDate: string;
    FBaseDocumentID: string;
    FBaseDocumentInfo: string;
    FBaseDocumentName: string;
    FBaseDocumentNumber: string;
    procedure SetBaseDocumentDate(AValue: string);
    procedure SetBaseDocumentID(AValue: string);
    procedure SetBaseDocumentInfo(AValue: string);
    procedure SetBaseDocumentName(AValue: string);
    procedure SetBaseDocumentNumber(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property BaseDocumentName:string read FBaseDocumentName write SetBaseDocumentName;
    property BaseDocumentNumber:string read FBaseDocumentNumber write SetBaseDocumentNumber;
    property BaseDocumentDate:string read FBaseDocumentDate write SetBaseDocumentDate;
    property BaseDocumentInfo:string read FBaseDocumentInfo write SetBaseDocumentInfo;
    property BaseDocumentID:string read FBaseDocumentID write SetBaseDocumentID;
  end;

  { TShipmentBaseList }

  TShipmentBaseList = class(TXmlSerializationObjectList) //%Таблица 5.48
  private
    function GetItem(AIndex: Integer): TShipmentBase; inline;
  public
    constructor Create;
    property Item[AIndex:Integer]:TShipmentBase read GetItem;
  end;

  { TSellerEmployee }

  TSellerEmployee =  class(TXmlSerializationObject)  //%Таблица 5.23
  private
    FOtherInformation: string;
    FPermisionPosition: string;
    FPerson: TPerson;
    FPosition: string;
    procedure SetOtherInformation(AValue: string);
    procedure SetPermisionPosition(AValue: string);
    procedure SetPosition(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property Position:string read FPosition write SetPosition;
    property OtherInformation:string read FOtherInformation write SetOtherInformation;
    property PermisionPosition:string read FPermisionPosition write SetPermisionPosition;
    property Person:TPerson read FPerson;
  end;

  { TOtherIssuerEmployee }

  TOtherIssuerEmployee = class(TXmlSerializationObject)  //%Таблица 5.25
  private
    FOrganizationName: string;
    FOtherInfo: string;
    FPerson: TPerson;
    FPosition: string;
    FPositionBase: string;
    FTransferBase: string;
    procedure SetOrganizationName(AValue: string);
    procedure SetOtherInfo(AValue: string);
    procedure SetPosition(AValue: string);
    procedure SetPositionBase(AValue: string);
    procedure SetTransferBase(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property Position:string read FPosition write SetPosition;
    property OtherInfo:string read FOtherInfo write SetOtherInfo;
    property OrganizationName:string read FOrganizationName write SetOrganizationName;
    property TransferBase:string read FTransferBase write SetTransferBase;
    property PositionBase:string read FPositionBase write SetPositionBase;
    property Person:TPerson read FPerson;
  end;

  { TPhysicalPersonTransfer }

  TPhysicalPersonTransfer =  class(TXmlSerializationObject)  //%Таблица 5.26
  private
    FOtherInfo: string;
    FPerson: TPerson;
    FTransferBase: string;
    procedure SetOtherInfo(AValue: string);
    procedure SetTransferBase(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property OtherInfo:string read FOtherInfo write SetOtherInfo;
    property TransferBase:string read FTransferBase write SetTransferBase;
    property Person:TPerson read FPerson;
  end;

  { TOtherIssuer }

  TOtherIssuer =  class(TXmlSerializationObject)  //%Таблица 5.24
  private
    FOtherIssuerEmployee: TOtherIssuerEmployee;
    FPhysicalPersonTransfer: TPhysicalPersonTransfer;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property OtherIssuerEmployee:TOtherIssuerEmployee read FOtherIssuerEmployee;
    property PhysicalPersonTransfer:TPhysicalPersonTransfer read FPhysicalPersonTransfer;
  end;

  { TTransferEmployee }

  TTransferEmployee =  class(TXmlSerializationObject)  //%Таблица 5.22
  private
    FOtherIssuer: TOtherIssuer;
    FSellerEmployee: TSellerEmployee;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property SellerEmployee:TSellerEmployee read FSellerEmployee;
    property OtherIssuer:TOtherIssuer read FOtherIssuer;
  end;

  { TWaybill }

  TWaybill = class(TXmlSerializationObject)  //%Таблица 5.28
  private
    FTransferDocumentDate: string;
    FTransferDocumentNumber: string;
    procedure SetTransferDocumentDate(AValue: string);
    procedure SetTransferDocumentNumber(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property TransferDocumentNumber:string read FTransferDocumentNumber write SetTransferDocumentNumber;
    property TransferDocumentDate:string read FTransferDocumentDate write SetTransferDocumentDate;
  end;

  { TWaybillList }

  TWaybillList = class(TXmlSerializationObjectList) //%Таблица 5.28
  private
    function GetItem(AIndex: Integer): TWaybill; inline;
  public
    constructor Create;
    property Item[AIndex:Integer]:TWaybill read GetItem;
  end;


  { TTransportationCargo }

  TTransportationCargo = class(TXmlSerializationObject)  //%Таблица 5.27
  private
    FCarrier: TOrganizationInfo;
    FTransferTextInfo: string;
    FWaybills: TWaybillList;
    procedure SetTransferTextInfo(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property TransferTextInfo:string read FTransferTextInfo write SetTransferTextInfo;
    property Waybills:TWaybillList read FWaybills;
    property Carrier:TOrganizationInfo read FCarrier;
  end;

  { TCreatedThing }

  TCreatedThing = class(TXmlSerializationObject)  //%Таблица 5.29
  private
    FCreatedThingInfo: string;
    FCreatedThingTransferDate: string;
    procedure SetCreatedThingInfo(AValue: string);
    procedure SetCreatedThingTransferDate(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property CreatedThingTransferDate:string read FCreatedThingTransferDate write SetCreatedThingTransferDate;
    property CreatedThingInfo:string read FCreatedThingInfo write SetCreatedThingInfo;
  end;

  { TItemsTransferInfo }

  TItemsTransferInfo = class(TXmlSerializationObject)  //%Таблица 5.21
  private
    FCreatedThing: TCreatedThing;
    FDateBegin: string;
    FDateEnd: string;
    FOperationInfo: string;
    FOperationType: string;
    FTransferBase: TShipmentBaseList;
    FTransferDate: string;
    FTransferEmployee: TTransferEmployee;
    FTransportationCargo: TTransportationCargo;
    procedure SetDateBegin(AValue: string);
    procedure SetDateEnd(AValue: string);
    procedure SetOperationInfo(AValue: string);
    procedure SetOperationType(AValue: string);
    procedure SetTransferDate(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property OperationInfo:string read FOperationInfo write SetOperationInfo;
    property OperationType:string read FOperationType write SetOperationType;
    property TransferDate:string read FTransferDate write SetTransferDate;
    property DateBegin:string read FDateBegin write SetDateBegin;
    property DateEnd:string read FDateEnd write SetDateEnd;
    property TransferBase:TShipmentBaseList read FTransferBase;
    property TransferEmployee:TTransferEmployee read FTransferEmployee;
    property TransportationCargo:TTransportationCargo read FTransportationCargo;
    property CreatedThing:TCreatedThing read FCreatedThing;
  end;

  { TTransferInfo }

  TTransferInfo = class(TXmlSerializationObject)  //%Таблица 5.20
  private
    FAdditionalInfo3: TAdditionalInfo3;
    FItemsTransferInfo: TItemsTransferInfo;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property ItemsTransferInfo:TItemsTransferInfo read FItemsTransferInfo;
    property AdditionalInfo3:TAdditionalInfo3 read FAdditionalInfo3;
  end;

implementation


{ TCreatedThing }

procedure TCreatedThing.SetCreatedThingInfo(AValue: string);
begin
  if FCreatedThingInfo=AValue then Exit;
  FCreatedThingInfo:=AValue;
  ModifiedProperty('CreatedThingInfo');
end;

procedure TCreatedThing.SetCreatedThingTransferDate(AValue: string);
begin
  if FCreatedThingTransferDate=AValue then Exit;
  FCreatedThingTransferDate:=AValue;
  ModifiedProperty('CreatedThingTransferDate');
end;

procedure TCreatedThing.InternalRegisterPropertys;
begin
  RegisterProperty('CreatedThingTransferDate', 'ДатаПерВещ', 'Н', 'Дата передачи вещи, изготовленной по договору подряда', 10, 10);
  RegisterProperty('CreatedThingInfo', 'СвПерВещ', 'Н', 'Сведения о передаче', 1, 1000);
end;

procedure TCreatedThing.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TCreatedThing.Destroy;
begin
  inherited Destroy;
end;

{ TWaybill }

procedure TWaybill.SetTransferDocumentDate(AValue: string);
begin
  if FTransferDocumentDate=AValue then Exit;
  FTransferDocumentDate:=AValue;
  ModifiedProperty('TransferDocumentDate');
end;

procedure TWaybill.SetTransferDocumentNumber(AValue: string);
begin
  if FTransferDocumentNumber=AValue then Exit;
  FTransferDocumentNumber:=AValue;
  ModifiedProperty('TransferDocumentNumber');
end;

procedure TWaybill.InternalRegisterPropertys;
begin
  RegisterProperty('TransferDocumentNumber', 'НомТранНакл', 'О', 'Номер транспортной накладной', 1, 255);
  RegisterProperty('TransferDocumentDate', 'ДатаТранНакл', 'О', 'Дата транспортной накладной', 10, 10);
end;

procedure TWaybill.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TWaybill.Destroy;
begin
  inherited Destroy;
end;

{ TWaybillList }

function TWaybillList.GetItem(AIndex: Integer): TWaybill;
begin
  Result:=TWaybill(InternalGetItem(AIndex));
end;

constructor TWaybillList.Create;
begin
  inherited Create(TWaybill)
end;

{ TTransportationCargo }

procedure TTransportationCargo.SetTransferTextInfo(AValue: string);
begin
  if FTransferTextInfo=AValue then Exit;
  FTransferTextInfo:=AValue;
  ModifiedProperty('TransferTextInfo');
end;

procedure TTransportationCargo.InternalRegisterPropertys;
begin
  RegisterProperty('TransferTextInfo', 'СвТранГруз', 'Н', 'Сведения о транспортировке и грузе', 1, 1000);
  RegisterProperty('Waybills', 'ТранНакл', 'НМ', 'Транспортная накладная', -1, -1);
  RegisterProperty('Carrier', 'Перевозчик', 'Н', 'Перевозчик', -1, -1);
end;

procedure TTransportationCargo.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FWaybills:=TWaybillList.Create;
  FCarrier:=TOrganizationInfo.Create;
end;

destructor TTransportationCargo.Destroy;
begin
  FreeAndNil(FWaybills);
  FreeAndNil(FCarrier);
  inherited Destroy;
end;

{ TPhysicalPersonTransfer }

procedure TPhysicalPersonTransfer.SetOtherInfo(AValue: string);
begin
  if FOtherInfo=AValue then Exit;
  FOtherInfo:=AValue;
  ModifiedProperty('OtherInfo');
end;

procedure TPhysicalPersonTransfer.SetTransferBase(AValue: string);
begin
  if FTransferBase=AValue then Exit;
  FTransferBase:=AValue;
  ModifiedProperty('TransferBase');
end;

procedure TPhysicalPersonTransfer.InternalRegisterPropertys;
begin
  RegisterProperty('OtherInfo', 'ИныеСвед', 'Н', 'Иные сведения, идентифицирующие физическое лицо', 1, 255);
  RegisterProperty('TransferBase', 'ОснДоверФЛ', 'Н', 'Основание, по которому физическому лицу доверена отгрузка товаров (передача результатов работ), передача имущественных прав (предъявление оказанных услуг)', 1, 120);
  RegisterProperty('Person', 'ФИО', 'Фамилия, имя, отчество', 'О', -1, -1);
end;

procedure TPhysicalPersonTransfer.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TPhysicalPersonTransfer.Destroy;
begin
  inherited Destroy;
end;

{ TOtherIssuerEmployee }

procedure TOtherIssuerEmployee.SetOrganizationName(AValue: string);
begin
  if FOrganizationName=AValue then Exit;
  FOrganizationName:=AValue;
  ModifiedProperty('OrganizationName');
end;

procedure TOtherIssuerEmployee.SetOtherInfo(AValue: string);
begin
  if FOtherInfo=AValue then Exit;
  FOtherInfo:=AValue;
  ModifiedProperty('OtherInfo');
end;

procedure TOtherIssuerEmployee.SetPosition(AValue: string);
begin
  if FPosition=AValue then Exit;
  FPosition:=AValue;
  ModifiedProperty('Position');
end;

procedure TOtherIssuerEmployee.SetPositionBase(AValue: string);
begin
  if FPositionBase=AValue then Exit;
  FPositionBase:=AValue;
  ModifiedProperty('PositionBase');
end;

procedure TOtherIssuerEmployee.SetTransferBase(AValue: string);
begin
  if FTransferBase=AValue then Exit;
  FTransferBase:=AValue;
  ModifiedProperty('TransferBase');
end;

procedure TOtherIssuerEmployee.InternalRegisterPropertys;
begin
  RegisterProperty('Position', 'Должность', 'О', 'Должность', 1, 128);
  RegisterProperty('OtherInfo', 'ИныеСвед', 'Н', 'ИныеСвед', 1, 255);
  RegisterProperty('OrganizationName', 'НаимОргПер', 'О', 'Наименование организации', 1, 128);
  RegisterProperty('TransferBase', 'ОснДоверОргПер', 'Н', 'Основание, по которому организации доверена отгрузка товаров (передача результатов работ), передача имущественных прав (предъявление оказанных услуг)', 1, 120);
  RegisterProperty('PositionBase', 'ОснПолнПредПер', 'Н', 'Основание полномочий представителя организации на отгрузку товаров (передачу результатов работ), передачу имущественных прав (предъявление оказанных услуг)', 1, 120);
  RegisterProperty('Person', 'ФИО', 'О', 'Фамилия, имя, отчество', -1, -1);
end;

procedure TOtherIssuerEmployee.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TOtherIssuerEmployee.Destroy;
begin
  inherited Destroy;
end;

{ TOtherIssuer }

procedure TOtherIssuer.InternalRegisterPropertys;
begin
  RegisterProperty('OtherIssuerEmployee', 'ПредОргПер', 'О', 'Представитель организации, которой доверена отгрузка товаров (передача результатов работ), передача имущественных прав (предъявление оказанных услуг)', -1, -1);
  RegisterProperty('PhysicalPersonTransfer', 'ФЛПер', 'О', 'Физическое лицо, которому доверена отгрузка товаров (передача результатов работ), передача имущественных прав (предъявление оказанных услуг)', -1, -1);
end;

procedure TOtherIssuer.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FOtherIssuerEmployee:=TOtherIssuerEmployee.Create;
  FPhysicalPersonTransfer:=TPhysicalPersonTransfer.Create;
end;

destructor TOtherIssuer.Destroy;
begin
  FreeAndNil(FOtherIssuerEmployee);
  FreeAndNil(FPhysicalPersonTransfer);
  inherited Destroy;
end;

{ TSellerEmployee }

procedure TSellerEmployee.SetOtherInformation(AValue: string);
begin
  if FOtherInformation=AValue then Exit;
  FOtherInformation:=AValue;
  ModifiedProperty('OtherInformation');
end;

procedure TSellerEmployee.SetPermisionPosition(AValue: string);
begin
  if FPermisionPosition=AValue then Exit;
  FPermisionPosition:=AValue;
  ModifiedProperty('PermisionPosition');
end;

procedure TSellerEmployee.SetPosition(AValue: string);
begin
  if FPosition=AValue then Exit;
  FPosition:=AValue;
  ModifiedProperty('Position');
end;

procedure TSellerEmployee.InternalRegisterPropertys;
begin
  RegisterProperty('Position', 'Должность', 'О', 'Должность', 1, 128);
  RegisterProperty('OtherInformation', 'ИныеСвед', 'Н', 'Иные сведения, идентифицирующие физическое лицо', 1, 255);
  RegisterProperty('PermisionPosition', 'ОснПолн', 'Н', 'Основание полномочий (доверия)', 1, 120);
  RegisterProperty('Person', 'ФИО', 'О', 'Фамилия, имя, отчество', -1, -1);
end;

procedure TSellerEmployee.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FPerson:=TPerson.Create;
end;

destructor TSellerEmployee.Destroy;
begin
  FreeAndNil(FPerson);
  inherited Destroy;
end;

{ TTransferEmployee }

procedure TTransferEmployee.InternalRegisterPropertys;
begin
  RegisterProperty('SellerEmployee', 'РабОргПрод', 'О', 'Работник организации продавца', -1, -1);
  RegisterProperty('OtherIssuer', 'ИнЛицо', 'О', 'Иное лицо', -1, -1);
end;

procedure TTransferEmployee.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FSellerEmployee:=TSellerEmployee.Create;
end;

destructor TTransferEmployee.Destroy;
begin
  FreeAndNil(FSellerEmployee);
  inherited Destroy;
end;

{ TShipmentBaseList }

function TShipmentBaseList.GetItem(AIndex: Integer): TShipmentBase;
begin
  Result:=TShipmentBase(InternalGetItem(AIndex));
end;

constructor TShipmentBaseList.Create;
begin
  inherited Create(TShipmentBase)
end;

{ TShipmentBase }

procedure TShipmentBase.SetBaseDocumentDate(AValue: string);
begin
  if FBaseDocumentDate=AValue then Exit;
  FBaseDocumentDate:=AValue;
  ModifiedProperty('BaseDocumentDate');
end;

procedure TShipmentBase.SetBaseDocumentID(AValue: string);
begin
  if FBaseDocumentID=AValue then Exit;
  FBaseDocumentID:=AValue;
  ModifiedProperty('BaseDocumentID');
end;

procedure TShipmentBase.SetBaseDocumentInfo(AValue: string);
begin
  if FBaseDocumentInfo=AValue then Exit;
  FBaseDocumentInfo:=AValue;
  ModifiedProperty('BaseDocumentInfo');
end;

procedure TShipmentBase.SetBaseDocumentName(AValue: string);
begin
  if FBaseDocumentName=AValue then Exit;
  FBaseDocumentName:=AValue;
  ModifiedProperty('BaseDocumentName');
end;

procedure TShipmentBase.SetBaseDocumentNumber(AValue: string);
begin
  if FBaseDocumentNumber=AValue then Exit;
  FBaseDocumentNumber:=AValue;
  ModifiedProperty('BaseDocumentNumber');
end;

procedure TShipmentBase.InternalRegisterPropertys;
begin
  RegisterProperty('BaseDocumentName', 'НаимОсн', 'О', 'Наименование документа - основания', 1, 255);
  RegisterProperty('BaseDocumentNumber', 'НомОсн', 'Н', 'Номер документа - основания', 1, 255);
  RegisterProperty('BaseDocumentDate', 'ДатаОсн', 'Н', 'Дата документа - основания', 10, 10);
  RegisterProperty('BaseDocumentInfo', 'ДопСвОсн', 'Н', 'Дополнительные сведения', 1, 1000);
  RegisterProperty('BaseDocumentID', 'ИдентОсн', 'Н', 'Идентификатор документа - основания', 1, 255);
end;

procedure TShipmentBase.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TShipmentBase.Destroy;
begin
  inherited Destroy;
end;

{ TItemsTransferInfo }

procedure TItemsTransferInfo.SetDateBegin(AValue: string);
begin
  if FDateBegin=AValue then Exit;
  FDateBegin:=AValue;
  ModifiedProperty('DateBegin');
end;

procedure TItemsTransferInfo.SetDateEnd(AValue: string);
begin
  if FDateEnd=AValue then Exit;
  FDateEnd:=AValue;
  ModifiedProperty('DateEnd');
end;

procedure TItemsTransferInfo.SetOperationInfo(AValue: string);
begin
  if FOperationInfo=AValue then Exit;
  FOperationInfo:=AValue;
  ModifiedProperty('OperationInfo');
end;

procedure TItemsTransferInfo.SetOperationType(AValue: string);
begin
  if FOperationType=AValue then Exit;
  FOperationType:=AValue;
  ModifiedProperty('OperationType');
end;

procedure TItemsTransferInfo.SetTransferDate(AValue: string);
begin
  if FTransferDate=AValue then Exit;
  FTransferDate:=AValue;
  ModifiedProperty('TransferDate');
end;

procedure TItemsTransferInfo.InternalRegisterPropertys;
begin
  RegisterProperty('OperationInfo', 'СодОпер', 'О', 'Содержание операции', 1, 255);
  RegisterProperty('OperationType', 'ВидОпер', 'Н', 'Вид операции', 1, 255);
  RegisterProperty('TransferDate', 'ДатаПер', 'Н', 'Дата отгрузки товаров (передачи результатов работ), передачи имущественных прав (предъявления оказанных услуг)', 10, 10);
  RegisterProperty('DateBegin', 'ДатаНач', 'Н', 'Дата начала периода оказания услуг (выполнения работ, поставки товаров)', 10, 10);
  RegisterProperty('DateEnd', 'ДатаОкон', 'Н', 'Дата окончания периода оказания услуг (выполнения работ, поставки товаров)', 10, 10);
  RegisterProperty('TransferBase', 'ОснПер', 'ОМ', 'Основание отгрузки товаров (передачи результатов работ), передачи имущественных прав (предъявления оказанных услуг)', -1, -1);
  RegisterProperty('TransferEmployee', 'СвЛицПер', 'Н', 'Сведения о лице, передавшем товар (груз)', -1, -1);
  RegisterProperty('TransportationCargo', 'ТранГруз', 'Н', 'Транспортировка и груз', -1, -1);
  RegisterProperty('CreatedThing', 'СвПерВещи', 'Н', 'Сведения о передаче вещи, изготовленной по договору подряда', -1, -1);
end;

procedure TItemsTransferInfo.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FTransferBase:=TShipmentBaseList.Create;
  FTransferEmployee:=TTransferEmployee.Create;
  FTransportationCargo:=TTransportationCargo.Create;
  FCreatedThing:=TCreatedThing.Create;
end;

destructor TItemsTransferInfo.Destroy;
begin
  FreeAndNil(FTransferBase);
  FreeAndNil(FTransferEmployee);
  FreeAndNil(FTransportationCargo);
  FreeAndNil(FCreatedThing);
  inherited Destroy;
end;

{ TTransferInfo }

procedure TTransferInfo.InternalRegisterPropertys;
begin
  RegisterProperty('ItemsTransferInfo', 'СвПер', 'О', 'Сведения о передаче (сдаче) товаров (результатов работ), имущественных прав (о предъявлении оказанных услуг)', -1, -1);
  RegisterProperty('AdditionalInfo3', 'ИнфПолФХЖЗ', 'Н', 'Информационное поле факта хозяйственной жизни 3', -1, -1);
end;

procedure TTransferInfo.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FItemsTransferInfo:=TItemsTransferInfo.Create;
  FAdditionalInfo3:=TAdditionalInfo3.Create;
end;

destructor TTransferInfo.Destroy;
begin
  FreeAndNil(FItemsTransferInfo);
  FreeAndNil(FAdditionalInfo3);
  inherited Destroy;
end;

end.

