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
  Classes, SysUtils, xmlobject;
type

  { TExchangePerson }

  TExchangePerson = class(TXmlSerializationObject)   //%Таблица 7.22
  private
    FFirstName: string;
    FPatronymic: string;
    FSurname: string;
    procedure SetFirstName(AValue: string);
    procedure SetPatronymic(AValue: string);
    procedure SetSurname(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property Surname:string read FSurname write SetSurname;
    property FirstName:string read FFirstName write SetFirstName;
    property Patronymic:string read FPatronymic write SetPatronymic;
  end;

  { TCustomerEmployee }

  TCustomerEmployee = class(TXmlSerializationObject)   //%Таблица 7.10
  private
    FOtherInfo: string;
    FPerson: TExchangePerson;
    FPosition: string;
    FSignerPowersBase: string;
    procedure SetOtherInfo(AValue: string);
    procedure SetPosition(AValue: string);
    procedure SetSignerPowersBase(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property Position:string read FPosition write SetPosition;
    property OtherInfo:string read FOtherInfo write SetOtherInfo;
    property SignerPowersBase:string read FSignerPowersBase write SetSignerPowersBase;
    property Person:TExchangePerson read FPerson;
  end;

  { TAacceptanceLegalEntityEmployee }

  TAacceptanceLegalEntityEmployee = class(TXmlSerializationObject)   //%Таблица 7.12
  private
    FOrganization: string;
    FOrgSignerPowersBase: string;
    FOtherInfo: string;
    FPerson: TExchangePerson;
    FPosition: string;
    FSignerPowersBase: string;
    procedure SetOrganization(AValue: string);
    procedure SetOrgSignerPowersBase(AValue: string);
    procedure SetOtherInfo(AValue: string);
    procedure SetPosition(AValue: string);
    procedure SetSignerPowersBase(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property Position:string read FPosition write SetPosition;
    property OtherInfo:string read FOtherInfo write SetOtherInfo;
    property Organization:string read FOrganization write SetOrganization;
    property OrgSignerPowersBase:string read FOrgSignerPowersBase write SetOrgSignerPowersBase;
    property SignerPowersBase:string read FSignerPowersBase write SetSignerPowersBase;
    property Person:TExchangePerson read FPerson;
  end;

  { TIndividualAcceptanceGoods }

  TIndividualAcceptanceGoods = class(TXmlSerializationObject)   //%Таблица 7.13
  private
    FOtherInfo: string;
    FPerson: TExchangePerson;
    FSignerPowersBase: string;
    procedure SetOtherInfo(AValue: string);
    procedure SetSignerPowersBase(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property SignerPowersBase:string read FSignerPowersBase write SetSignerPowersBase;
    property OtherInfo:string read FOtherInfo write SetOtherInfo;
    property Person:TExchangePerson read FPerson;
  end;

  { TExchangeTextInfo }

  TExchangeTextInfo = class(TXmlSerializationObject)   //%Таблица 7.15
  private
    FID: string;
    FValue: string;
    procedure SetID(AValue: string);
    procedure SetValue(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property ID:string read FID write SetID;
    property Value:string read FValue write SetValue;
  end;

  { TExchangeTextInfoList }

  TExchangeTextInfoList = class(TXmlSerializationObjectList) //%Таблица 7.15
  private
    function GetItem(AIndex: Integer): TExchangeTextInfo; inline;
  public
    constructor Create;
    function CreateChild:TExchangeTextInfo;
    property Item[AIndex:Integer]:TExchangeTextInfo read GetItem; default;
  end;

  { TAdditionalInfoId4 }

  TAdditionalInfoId4 = class(TXmlSerializationObject)   //%Таблица 7.14
  private
    FFileID: string;
    FTextInfo: TExchangeTextInfoList;
    procedure SetFileID(AValue: string);
    procedure SetTextInfo(AValue: TExchangeTextInfoList);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property FileID:string read FFileID write SetFileID;
    property TextInfo:TExchangeTextInfoList read FTextInfo write SetTextInfo;
  end;

  { TExchangeOtherIssuer }

  TExchangeOtherIssuer = class(TXmlSerializationObject)   //%Таблица 7.11
  private
    FIndividualAcceptanceGoods: TIndividualAcceptanceGoods;
    FLegalEntityEmployee: TAacceptanceLegalEntityEmployee;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property LegalEntityEmployee:TAacceptanceLegalEntityEmployee read FLegalEntityEmployee;
    property IndividualAcceptanceGoods:TIndividualAcceptanceGoods read FIndividualAcceptanceGoods;
  end;


  { TCargoPerson }

  TCargoPerson = class(TXmlSerializationObject)   //%Таблица 7.9
  private
    FCustomerEmployee: TCustomerEmployee;
    FOtherIssuer: TExchangeOtherIssuer;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property CustomerEmployee:TCustomerEmployee read FCustomerEmployee;
    property OtherIssuer:TExchangeOtherIssuer read FOtherIssuer;
  end;

  { TOperationCode }

  TOperationCode = class(TXmlSerializationObject)   //%Таблица 7.8
  private
    FDocumentCode: string;
    FDocumentDate: string;
    FDocumentName: string;
    FDocumentNumber: string;
    FFileID: string;
    FItogCode: string;
    procedure SetDocumentCode(AValue: string);
    procedure SetDocumentDate(AValue: string);
    procedure SetDocumentName(AValue: string);
    procedure SetDocumentNumber(AValue: string);
    procedure SetFileID(AValue: string);
    procedure SetItogCode(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property ItogCode:string read FItogCode write SetItogCode;
    property DocumentName:string read FDocumentName write SetDocumentName;
    property DocumentCode:string read FDocumentCode write SetDocumentCode;
    property DocumentNumber:string read FDocumentNumber write SetDocumentNumber;
    property DocumentDate:string read FDocumentDate write SetDocumentDate;
    property FileID:string read FFileID write SetFileID;
  end;

  { TAcceptanceInformation }

  TAcceptanceInformation = class(TXmlSerializationObject)   //%Таблица 7.7
  private
    FAcceptanceDate: string;
    FCargoPerson: TCargoPerson;
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
    property CargoPerson:TCargoPerson read FCargoPerson;
  end;

  { TExchangeInformation }

  TExchangeInformation = class(TXmlSerializationObject)   //%Таблица 7.6
  private
    FAcceptanceInformation: TAcceptanceInformation;
    FAdditionalInfoId: TAdditionalInfoId4;
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
    property AdditionalInfoId:TAdditionalInfoId4 read FAdditionalInfoId;
  end;

implementation

{ TExchangeTextInfo }

procedure TExchangeTextInfo.SetID(AValue: string);
begin
  if FID=AValue then Exit;
  FID:=AValue;
  ModifiedProperty('ID');
end;

procedure TExchangeTextInfo.SetValue(AValue: string);
begin
  if FValue=AValue then Exit;
  FValue:=AValue;
  ModifiedProperty('Value');
end;

procedure TExchangeTextInfo.InternalRegisterPropertys;
begin
  RegisterProperty('ID', 'Идентиф', 'О', 'Идентификатор', 1, 50);
  RegisterProperty('Value', 'Значен', 'О', 'Значение', 1, 2000);
end;

procedure TExchangeTextInfo.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TExchangeTextInfo.Destroy;
begin
  inherited Destroy;
end;

{ TExchangeTextInfoList }

function TExchangeTextInfoList.GetItem(AIndex: Integer): TExchangeTextInfo;
begin
  Result:=TExchangeTextInfo(InternalGetItem(AIndex));
end;

constructor TExchangeTextInfoList.Create;
begin
  inherited Create(TExchangeTextInfo)
end;

function TExchangeTextInfoList.CreateChild: TExchangeTextInfo;
begin
  Result:=InternalAddObject as TExchangeTextInfo;
end;

{ TAdditionalInfoId4 }

procedure TAdditionalInfoId4.SetFileID(AValue: string);
begin
  if FFileID=AValue then Exit;
  FFileID:=AValue;
  ModifiedProperty('FileID');
end;

procedure TAdditionalInfoId4.SetTextInfo(AValue: TExchangeTextInfoList);
begin
  if FTextInfo=AValue then Exit;
  FTextInfo:=AValue;
  ModifiedProperty('TextInfo');
end;

procedure TAdditionalInfoId4.InternalRegisterPropertys;
begin
  RegisterProperty('FileID', 'ИдФайлИнфПол', 'Н', 'Идентификатор файла информационного поля', 36, 36);
  RegisterProperty('TextInfo', 'ТекстИнф', 'НМ', 'Текстовая информация', -1, -1);
end;

procedure TAdditionalInfoId4.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FTextInfo:=TExchangeTextInfoList.Create;
end;

destructor TAdditionalInfoId4.Destroy;
begin
  FreeAndNil(FTextInfo);
  inherited Destroy;
end;

{ TIndividualAcceptanceGoods }

procedure TIndividualAcceptanceGoods.SetOtherInfo(AValue: string);
begin
  if FOtherInfo=AValue then Exit;
  FOtherInfo:=AValue;
  ModifiedProperty('OtherInfo');
end;

procedure TIndividualAcceptanceGoods.SetSignerPowersBase(AValue: string);
begin
  if FSignerPowersBase=AValue then Exit;
  FSignerPowersBase:=AValue;
  ModifiedProperty('SignerPowersBase');
end;

procedure TIndividualAcceptanceGoods.InternalRegisterPropertys;
begin
  RegisterProperty('SignerPowersBase', 'ОснДоверФЛ', 'Н', 'Основание, по которому физическому лицу доверено принятие товаров (груза)', 1, 120);
  RegisterProperty('OtherInfo', 'ИныеСвед', 'Н', 'Иные сведения, идентифицирующие физическое лицо', 1, 255);
  RegisterProperty('Person', 'ФИО', 'О', 'Фамилия, имя, отчество', -1, -1);
end;

procedure TIndividualAcceptanceGoods.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FPerson:=TExchangePerson.Create;
end;

destructor TIndividualAcceptanceGoods.Destroy;
begin
  FreeAndNil(FPerson);
  inherited Destroy;
end;

{ TAacceptanceLegalEntityEmployee }

procedure TAacceptanceLegalEntityEmployee.SetOrganization(AValue: string);
begin
  if FOrganization=AValue then Exit;
  FOrganization:=AValue;
end;

procedure TAacceptanceLegalEntityEmployee.SetOrgSignerPowersBase(AValue: string
  );
begin
  if FOrgSignerPowersBase=AValue then Exit;
  FOrgSignerPowersBase:=AValue;
end;

procedure TAacceptanceLegalEntityEmployee.SetOtherInfo(AValue: string);
begin
  if FOtherInfo=AValue then Exit;
  FOtherInfo:=AValue;
end;

procedure TAacceptanceLegalEntityEmployee.SetPosition(AValue: string);
begin
  if FPosition=AValue then Exit;
  FPosition:=AValue;
end;

procedure TAacceptanceLegalEntityEmployee.SetSignerPowersBase(AValue: string);
begin
  if FSignerPowersBase=AValue then Exit;
  FSignerPowersBase:=AValue;
end;

procedure TAacceptanceLegalEntityEmployee.InternalRegisterPropertys;
begin
  RegisterProperty('Position', 'Должность', 'О', 'Должность', 1, 128);
  RegisterProperty('OtherInfo', 'ИныеСвед', 'Н', 'Иные сведения, идентифицирующие физическое лицо', 1, 255);
  RegisterProperty('Organization', 'НаимОргПрин', 'О', 'Наименование организации', 1, 128);
  RegisterProperty('OrgSignerPowersBase', 'ОснДоверОргПрин', 'Н', 'Основание, по которому организации доверено принятие товаров (груза)', 1, 120);
  RegisterProperty('SignerPowersBase', 'ОснПолнПредПрин', 'Н', 'Основание полномочий представителя организации на принятие товаров (груза)', 1, 120);
  RegisterProperty('Person', 'ФИО', 'О', 'Фамилия, имя, отчество', -1, -1);
end;

procedure TAacceptanceLegalEntityEmployee.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FPerson:=TExchangePerson.Create;
end;

destructor TAacceptanceLegalEntityEmployee.Destroy;
begin
  FreeAndNil(FPerson);
  inherited Destroy;
end;

{ TExchangeOtherIssuer }

procedure TExchangeOtherIssuer.InternalRegisterPropertys;
begin
  RegisterProperty('LegalEntityEmployee', 'ПредОргПрин', 'О', 'Представитель организации, которой доверено принятие товаров (груза)', -1, -1);
  RegisterProperty('IndividualAcceptanceGoods', 'ФЛПрин', 'О', 'Физическое лицо, которому доверено принятие товаров (груза)', -1, -1);
end;

procedure TExchangeOtherIssuer.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FLegalEntityEmployee:=TAacceptanceLegalEntityEmployee.Create;
  FIndividualAcceptanceGoods:=TIndividualAcceptanceGoods.Create;
end;

destructor TExchangeOtherIssuer.Destroy;
begin
  FreeAndNil(FLegalEntityEmployee);
  FreeAndNil(FIndividualAcceptanceGoods);
  inherited Destroy;
end;

{ TExchangePerson }

procedure TExchangePerson.SetFirstName(AValue: string);
begin
  if FFirstName=AValue then Exit;
  FFirstName:=AValue;
  ModifiedProperty('FirstName');
end;

procedure TExchangePerson.SetPatronymic(AValue: string);
begin
  if FPatronymic=AValue then Exit;
  FPatronymic:=AValue;
  ModifiedProperty('Patronymic');
end;

procedure TExchangePerson.SetSurname(AValue: string);
begin
  if FSurname=AValue then Exit;
  FSurname:=AValue;
  ModifiedProperty('Surname');
end;

procedure TExchangePerson.InternalRegisterPropertys;
begin
  RegisterProperty('Surname', 'Фамилия', 'О', 'Фамилия', 1, 60);
  RegisterProperty('FirstName', 'Имя', 'О', 'Имя', 1, 60);
  RegisterProperty('Patronymic', 'Отчество', 'Н', 'Отчество', 1, 60);
end;

procedure TExchangePerson.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TExchangePerson.Destroy;
begin
  inherited Destroy;
end;

{ TCustomerEmployee }

procedure TCustomerEmployee.SetOtherInfo(AValue: string);
begin
  if FOtherInfo=AValue then Exit;
  FOtherInfo:=AValue;
  ModifiedProperty('OtherInfo');
end;

procedure TCustomerEmployee.SetPosition(AValue: string);
begin
  if FPosition=AValue then Exit;
  FPosition:=AValue;
  ModifiedProperty('Position');
end;

procedure TCustomerEmployee.SetSignerPowersBase(AValue: string);
begin
  if FSignerPowersBase=AValue then Exit;
  FSignerPowersBase:=AValue;
  ModifiedProperty('SignerPowersBase');
end;

procedure TCustomerEmployee.InternalRegisterPropertys;
begin
  RegisterProperty('Position', 'Должность', 'О', 'Должность', 1, 128);
  RegisterProperty('OtherInfo', 'ИныеСвед', 'Н', 'Иные сведения, идентифицирующие физическое лицо', 1, 255);
  RegisterProperty('SignerPowersBase', 'ОснПолн', 'О', 'Основание полномочий (доверия)', 1, 120);
  RegisterProperty('Person', 'ФИО', 'О', 'Фамилия, имя, отчество', -1, -1);
end;

procedure TCustomerEmployee.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FPerson:=TExchangePerson.Create;
end;

destructor TCustomerEmployee.Destroy;
begin
  FreeAndNil(FPerson);
  inherited Destroy;
end;

{ TCargoPerson }

procedure TCargoPerson.InternalRegisterPropertys;
begin
  RegisterProperty('CustomerEmployee', 'РабОргПок', 'О', 'Работник организации покупателя', -1, -1);
  RegisterProperty('OtherIssuer', 'ИнЛицо', 'О', 'Иное лицо', -1, -1);
end;

procedure TCargoPerson.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FCustomerEmployee:=TCustomerEmployee.Create;
  FOtherIssuer:=TExchangeOtherIssuer.Create;
end;

destructor TCargoPerson.Destroy;
begin
  FreeAndNil(FCustomerEmployee);
  FreeAndNil(FOtherIssuer);
  inherited Destroy;
end;

{ TOperationCode }

procedure TOperationCode.SetDocumentName(AValue: string);
begin
  if FDocumentName=AValue then Exit;
  FDocumentName:=AValue;
  ModifiedProperty('DocumentName');
end;

procedure TOperationCode.SetDocumentCode(AValue: string);
begin
  if FDocumentCode=AValue then Exit;
  FDocumentCode:=AValue;
  ModifiedProperty('DocumentCode');
end;

procedure TOperationCode.SetDocumentDate(AValue: string);
begin
  if FDocumentDate=AValue then Exit;
  FDocumentDate:=AValue;
  ModifiedProperty('DocumentDate');
end;

procedure TOperationCode.SetDocumentNumber(AValue: string);
begin
  if FDocumentNumber=AValue then Exit;
  FDocumentNumber:=AValue;
  ModifiedProperty('DocumentNumber');
end;

procedure TOperationCode.SetFileID(AValue: string);
begin
  if FFileID=AValue then Exit;
  FFileID:=AValue;
  ModifiedProperty('FileID');
end;

procedure TOperationCode.SetItogCode(AValue: string);
begin
  if FItogCode=AValue then Exit;
  FItogCode:=AValue;
  ModifiedProperty('ItogCode');
end;

procedure TOperationCode.InternalRegisterPropertys;
begin
  RegisterProperty('ItogCode', 'КодИтога', 'ОК', 'Код, обозначающий итог приемки товара (работ, услуг, прав)', 1, 1);
  RegisterProperty('DocumentName', 'НаимДокРасх', 'Н', 'Наименование документа, оформляющего расхождения', 1, 255);
  RegisterProperty('DocumentCode', 'ВидДокРасх', 'НК', 'Код вида документа о расхождениях', 1, 1);
  RegisterProperty('DocumentNumber', 'НомДокРасх', 'Н', 'Номер документа покупателя о расхождениях', 1, 255);
  RegisterProperty('DocumentDate', 'ДатаДокРасх', 'Н', 'Дата документа о расхождениях', 10, 10);
  RegisterProperty('FileID', 'ИдФайлДокРасх', 'Н', 'Идентификатор файла обмена документа о расхождениях, сформированного покупателем', 1, 255);
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
  RegisterProperty('CargoPerson', 'СвЛицПрин', 'Н', 'Сведения о лице, принявшем товары (груз)', -1, -1);
end;

procedure TAcceptanceInformation.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FOperationCode:=TOperationCode.Create;
  FCargoPerson:=TCargoPerson.Create;
end;

destructor TAcceptanceInformation.Destroy;
begin
  FreeAndNil(FOperationCode);
  FreeAndNil(FCargoPerson);
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
  RegisterProperty('AdditionalInfoId', 'ИнфПолФХЖ4', 'Н', 'Информационное поле факта хозяйственной жизни 4', -1, -1);
end;

procedure TExchangeInformation.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FAcceptanceInformation:=TAcceptanceInformation.Create;
  FAdditionalInfoId:=TAdditionalInfoId4.Create;
end;

destructor TExchangeInformation.Destroy;
begin
  FreeAndNil(FAdditionalInfoId);
  FreeAndNil(FAcceptanceInformation);
  inherited Destroy;
end;

end.

