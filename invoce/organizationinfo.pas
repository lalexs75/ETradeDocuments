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

unit OrganizationInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xml_doc, AdressInfo;

type

  { TPerson }

  TPerson = class(TXmlSerializationObject)   //%Таблица 5.49
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

  { TIndividualEntrepreneurInformation }

  TIndividualEntrepreneurInformation = class(TXmlSerializationObject)  //%Таблица 5.43
  private
    FIndividualEntityRegistrationCertificate: string;
    FINN: string;
    FINNDef: string;
    FOtherInfo: string;
    FPerson: TPerson;
    procedure SetIndividualEntityRegistrationCertificate(AValue: string);
    procedure SetINN(AValue: string);
    procedure SetINNDef(AValue: string);
    procedure SetOtherInfo(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property INN:string read FINN write SetINN;
    property INNDef:string read FINNDef write SetINNDef;
    property IndividualEntityRegistrationCertificate:string read FIndividualEntityRegistrationCertificate write SetIndividualEntityRegistrationCertificate;
    property OtherInfo:string read FOtherInfo write SetOtherInfo;
    property Person:TPerson read FPerson;
  end;

  { TLegalEntityInformation }

  TLegalEntityInformation = class(TXmlSerializationObject) //%Таблица 5.35
  private
    FFullName: string;
    FINN: string;
    FINNDef: string;
    FKPP: string;
    procedure SetFullName(AValue: string);
    procedure SetINN(AValue: string);
    procedure SetINNDef(AValue: string);
    procedure SetKPP(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property FullName:string read FFullName write SetFullName;
    property INN:string read FINN write SetINN;
    property INNDef:string read FINNDef write SetINNDef;
    property KPP:string read FKPP write SetKPP;
  end;

  { TForeignEntityInformation }

  TForeignEntityInformation = class(TXmlSerializationObject) //%Таблица 5.36
  private
    FFullName: string;
    FIdentifier: string;
    FOtherInfo: string;
    procedure SetFullName(AValue: string);
    procedure SetIdentifier(AValue: string);
    procedure SetOtherInfo(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property FullName:string read FFullName write SetFullName;
    property Identifier:string read FIdentifier write SetIdentifier;
    property OtherInfo:string read FOtherInfo write SetOtherInfo;
  end;

  { TPhysicalPersonEntity }

  TPhysicalPersonEntity = class(TXmlSerializationObject) //%Таблица 5.44
  private
    FCertificateStateRegistration: string;
    FINN: string;
    FOtherInfo: string;
    FPerson: TPerson;
    procedure SetCertificateStateRegistration(AValue: string);
    procedure SetINN(AValue: string);
    procedure SetOtherInfo(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property CertificateStateRegistration:string read FCertificateStateRegistration write SetCertificateStateRegistration;
    property INN:string read FINN write SetINN;
    property OtherInfo:string read FOtherInfo write SetOtherInfo;
    property Person:TPerson read FPerson;
  end;

  { TIdentificationInformation }

  TIdentificationInformation = class(TXmlSerializationObject) //%Таблица 5.34
  private
    FForeignEntityInformation: TForeignEntityInformation;
    FIndividualEntrepreneurInformation: TIndividualEntrepreneurInformation;
    FLegalEntityInformation: TLegalEntityInformation;
    FPhysicalPerson: TPhysicalPersonEntity;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property IndividualEntrepreneurInformation:TIndividualEntrepreneurInformation read FIndividualEntrepreneurInformation;
    property LegalEntityInformation:TLegalEntityInformation read FLegalEntityInformation;
    property ForeignEntityInformation:TForeignEntityInformation read FForeignEntityInformation;
    property PhysicalPerson:TPhysicalPersonEntity read FPhysicalPerson;
  end;

  { TContactInfo }

  TContactInfo = class(TXmlSerializationObject)  //%Таблица 5.42
  private
    FEMail: string;
    FPhone: string;
    procedure SetEMail(AValue: string);
    procedure SetPhone(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property Phone:string read FPhone write SetPhone;
    property EMail:string read FEMail write SetEMail;
  end;

  { TBankInfo }

  TBankInfo = class(TXmlSerializationObject)  //%Таблица 5.38
  private
    FBIK: string;
    FKorrAcount: string;
    FName: string;
    procedure SetBIK(AValue: string);
    procedure SetKorrAcount(AValue: string);
    procedure SetName(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property Name:string read FName write SetName;
    property BIK:string read FBIK write SetBIK;
    property KorrAcount:string read FKorrAcount write SetKorrAcount;
  end;

  { TBankDetails }

  TBankDetails = class(TXmlSerializationObject)  //%Таблица 5.37
  private
    FAccount: string;
    FBankInfo: TBankInfo;
    procedure SetAccount(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property Account:string read FAccount write SetAccount;
    property BankInfo:TBankInfo read FBankInfo;
  end;

  { TOrganizationInfo }

  TOrganizationInfo = class(TXmlSerializationObject)  //%Таблица 5.33
  private
    FAdress: TAdress;
    FBankDetails: TBankDetails;
    FContactInfo: TContactInfo;
    FDepartment: string;
    FIdentificationInformation: TIdentificationInformation;
    FOKPO: string;
    FOrganizationOrPersonInfo: string;
    FShortName: string;
    procedure SetDepartment(AValue: string);
    procedure SetOKPO(AValue: string);
    procedure SetOrganizationOrPersonInfo(AValue: string);
    procedure SetShortName(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property OKPO:string read FOKPO write SetOKPO;
    property Department:string read FDepartment write SetDepartment;
    property OrganizationOrPersonInfo:string read FOrganizationOrPersonInfo write SetOrganizationOrPersonInfo;
    property ShortName:string read FShortName write SetShortName;
    property IdentificationInformation:TIdentificationInformation read FIdentificationInformation;
    property Adress:TAdress read FAdress;
    property ContactInfo:TContactInfo read FContactInfo;
    property BankDetails:TBankDetails read FBankDetails;
  end;

  { TOrganizationInfoList }

  TOrganizationInfoList = class(TXmlSerializationObjectList) //%Таблица 5.33
  private
    function GetItem(AIndex: Integer): TOrganizationInfo; inline;
  public
    constructor Create;
    property Item[AIndex:Integer]:TOrganizationInfo read GetItem; default;
  end;


  { TLegalEntityEmployee }

  TLegalEntityEmployee = class(TXmlSerializationObject)  //%Таблица 5.32
  private
    FInn: string;
    FOrganizationName: string;
    FOtherInfo: string;
    FPerson: TPerson;
    FPosition: string;
    FRegistrationInfo: string;
    procedure SetInn(AValue: string);
    procedure SetOrganizationName(AValue: string);
    procedure SetOtherInfo(AValue: string);
    procedure SetPosition(AValue: string);
    procedure SetRegistrationInfo(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property RegistrationInfo:string read FRegistrationInfo write SetRegistrationInfo;
    property Inn:string read FInn write SetInn;
    property OrganizationName:string read FOrganizationName write SetOrganizationName;
    property Position:string read FPosition write SetPosition;
    property OtherInfo:string read FOtherInfo write SetOtherInfo;
    property Person:TPerson read FPerson;
  end;

implementation

{ TLegalEntityEmployee }

procedure TLegalEntityEmployee.SetInn(AValue: string);
begin
  if FInn=AValue then Exit;
  FInn:=AValue;
  ModifiedProperty('Inn');
end;

procedure TLegalEntityEmployee.SetOrganizationName(AValue: string);
begin
  if FOrganizationName=AValue then Exit;
  FOrganizationName:=AValue;
  ModifiedProperty('OrganizationName');
end;

procedure TLegalEntityEmployee.SetOtherInfo(AValue: string);
begin
  if FOtherInfo=AValue then Exit;
  FOtherInfo:=AValue;
  ModifiedProperty('OtherInfo');
end;

procedure TLegalEntityEmployee.SetPosition(AValue: string);
begin
  if FPosition=AValue then Exit;
  FPosition:=AValue;
  ModifiedProperty('Position');
end;

procedure TLegalEntityEmployee.SetRegistrationInfo(AValue: string);
begin
  if FRegistrationInfo=AValue then Exit;
  FRegistrationInfo:=AValue;
  ModifiedProperty('RegistrationInfo');
end;

procedure TLegalEntityEmployee.InternalRegisterPropertys;
begin
  RegisterProperty('RegistrationInfo', 'ГосРегИПВыдДов', 'Н', 'Реквизиты свидетельства о государственной регистрации индивидуального предпринимателя, выдавшего доверенность организации на подписание счета-фактуры', 1, 100);
  RegisterProperty('Inn', 'ИННЮЛ', 'О', 'ИНН юридического лица', 10, 10);
  RegisterProperty('OrganizationName', 'НаимОрг', 'Н', 'Наименование', 1, 1000);
  RegisterProperty('Position', 'Должн', 'О', 'Должность', 1, 128);
  RegisterProperty('OtherInfo', 'ИныеСвед', 'Н', 'Иные сведения, идентифицирующие физическое лицо', 1, 255);
  RegisterProperty('Person', 'ФИО', 'О', 'Фамилия, имя, отчество', -1, -1);
end;

procedure TLegalEntityEmployee.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FPerson:=TPerson.Create;
end;

destructor TLegalEntityEmployee.Destroy;
begin
  FreeAndNil(FPerson);
  inherited Destroy;
end;

{ TBankInfo }

procedure TBankInfo.SetBIK(AValue: string);
begin
  if FBIK=AValue then Exit;
  FBIK:=AValue;
  ModifiedProperty('BIK');
end;

procedure TBankInfo.SetKorrAcount(AValue: string);
begin
  if FKorrAcount=AValue then Exit;
  FKorrAcount:=AValue;
  ModifiedProperty('KorrAcount');
end;

procedure TBankInfo.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
  ModifiedProperty('Name');
end;

procedure TBankInfo.InternalRegisterPropertys;
begin
  RegisterProperty('Name', 'НаимБанк', 'Н', 'Наименование банка', 1, 1000);
  RegisterProperty('BIK', 'БИК', 'НК', 'Банковский идентификационный код (БИК) в соответствии со "Справочником БИК РФ"', 9, 9);
  RegisterProperty('KorrAcount', 'КорСчет', 'Н', 'Корреспондентский счет банка', 1, 20);
end;

procedure TBankInfo.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TBankInfo.Destroy;
begin
  inherited Destroy;
end;

{ TBankDetails }

procedure TBankDetails.SetAccount(AValue: string);
begin
  if FAccount=AValue then Exit;
  FAccount:=AValue;
  ModifiedProperty('Account');
end;

procedure TBankDetails.InternalRegisterPropertys;
begin
  RegisterProperty('Account', 'НомерСчета', 'Н', 'Номер банковского счета', 1, 20);
  RegisterProperty('BankInfo', 'СвБанк', 'Н', 'Сведения о банке', -1, -1)
end;

procedure TBankDetails.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FBankInfo:=TBankInfo.Create;
end;

destructor TBankDetails.Destroy;
begin
  FreeAndNil(FBankInfo);
  inherited Destroy;
end;

{ TContactInfo }

procedure TContactInfo.SetEMail(AValue: string);
begin
  if FEmail=AValue then Exit;
  FEmail:=AValue;
  ModifiedProperty('Email');
end;

procedure TContactInfo.SetPhone(AValue: string);
begin
  if FPhone=AValue then Exit;
  FPhone:=AValue;
  ModifiedProperty('Phone');
end;

procedure TContactInfo.InternalRegisterPropertys;
begin
  RegisterProperty('Phone', 'Тлф', 'Н', 'Номер контактного телефона/факс', 1, 255);
  RegisterProperty('EMail', 'ЭлПочта', 'Н', 'Адрес электронной почты', 1, 255);
end;

procedure TContactInfo.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TContactInfo.Destroy;
begin
  inherited Destroy;
end;

{ TPhysicalPersonEntity }

procedure TPhysicalPersonEntity.SetCertificateStateRegistration(AValue: string);
begin
  if FCertificateStateRegistration=AValue then Exit;
  FCertificateStateRegistration:=AValue;
  ModifiedProperty('CertificateStateRegistration');
end;

procedure TPhysicalPersonEntity.SetINN(AValue: string);
begin
  if FINN=AValue then Exit;
  FINN:=AValue;
  ModifiedProperty('INN');
end;

procedure TPhysicalPersonEntity.SetOtherInfo(AValue: string);
begin
  if FOtherInfo=AValue then Exit;
  FOtherInfo:=AValue;
  ModifiedProperty('OtherInfo');
end;

procedure TPhysicalPersonEntity.InternalRegisterPropertys;
begin
  RegisterProperty('CertificateStateRegistration', 'ГосРегИПВыдДов', 'Н', 'Реквизиты свидетельства о государственной регистрации индивидуального предпринимателя, выдавшего доверенность физическому лицу на подписание счета-фактуры', 1, 100);
  RegisterProperty('INN', 'ИННФЛ', 'Н', 'ИНН физического лица', 12, 12);
  RegisterProperty('OtherInfo', 'ИныеСвед', 'Н', 'Иные сведения, идентифицирующие физическое лицо', 1, 255);
  RegisterProperty('Person', 'ФИО', 'О', 'Фамилия, имя, отчество', -1, -1);
end;

procedure TPhysicalPersonEntity.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FPerson:=TPerson.Create;
end;

destructor TPhysicalPersonEntity.Destroy;
begin
  FreeAndNil(FPerson);
  inherited Destroy;
end;

{ TForeignEntityInformation }

procedure TForeignEntityInformation.SetFullName(AValue: string);
begin
  if FFullName=AValue then Exit;
  FFullName:=AValue;
  ModifiedProperty(FullName);
end;

procedure TForeignEntityInformation.SetIdentifier(AValue: string);
begin
  if FIdentifier=AValue then Exit;
  FIdentifier:=AValue;
  ModifiedProperty('Identifier');
end;

procedure TForeignEntityInformation.SetOtherInfo(AValue: string);
begin
  if FOtherInfo=AValue then Exit;
  FOtherInfo:=AValue;
  ModifiedProperty('OtherInfo');
end;

procedure TForeignEntityInformation.InternalRegisterPropertys;
begin
  RegisterProperty('FullName', 'НаимОрг', 'О', 'Наименование полное', 1, 1000);
  RegisterProperty('Identifier', 'Идентиф', 'Н', 'Идентификатор юридического лица', 1, 255);
  RegisterProperty('OtherInfo', 'ИныеСвед', 'Н', 'Иные сведения, идентифицирующие юридическое лицо', 1, 255);
end;

procedure TForeignEntityInformation.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TForeignEntityInformation.Destroy;
begin
  inherited Destroy;
end;

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

procedure TLegalEntityInformation.SetINNDef(AValue: string);
begin
  if FINNDef=AValue then Exit;
  FINNDef:=AValue;
  ModifiedProperty('INNDef');
end;

procedure TLegalEntityInformation.SetKPP(AValue: string);
begin
  if FKPP=AValue then Exit;
  FKPP:=AValue;
  ModifiedProperty('KPP');
end;

procedure TLegalEntityInformation.InternalRegisterPropertys;
begin
  RegisterProperty('FullName', 'НаимОрг', 'О', 'Наименование полное', 1, 1000);
  RegisterProperty('INN', 'ИННЮЛ', 'Н', 'ИНН', 10, 10);
  RegisterProperty('INNDef', 'ИННЮЛ', 'Н', 'ИНН при составлении документа с Функция=СЧФ или Функция=СЧФДОП налоговым агентом, указанным в пунктах 2 и 3 статьи 161 НК РФ, в части информации о продавце/грузоотправителе/грузополучателе или для документа с Функция=СЧФ, выставляемом при получении оплаты, частичной оплаты в счет предстоящих поставок товаров (выполнения работ, оказания услуг), передачи имущественных прав, в части информации о грузоотправителе/грузополучателе', 1, 1);
  RegisterProperty('KPP', 'КПП', 'Н', 'КПП', 9, 9);
end;

procedure TLegalEntityInformation.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TLegalEntityInformation.Destroy;
begin
  inherited Destroy;
end;

{ TPerson }

procedure TPerson.SetFirstName(AValue: string);
begin
  if FFirstName=AValue then Exit;
  FFirstName:=AValue;
  ModifiedProperty('FirstName');
end;

procedure TPerson.SetPatronymic(AValue: string);
begin
  if FPatronymic=AValue then Exit;
  FPatronymic:=AValue;
  ModifiedProperty('Patronymic');
end;

procedure TPerson.SetSurname(AValue: string);
begin
  if FSurname=AValue then Exit;
  FSurname:=AValue;
  ModifiedProperty('Surname');
end;

procedure TPerson.InternalRegisterPropertys;
begin
  RegisterProperty('FirstName', 'Фамилия', 'О', 'Фамилия', 1, 60);
  RegisterProperty('Patronymic', 'Имя', 'О', 'Имя', 1, 60);
  RegisterProperty('Surname', 'Отчество', 'Н', 'Отчество', 1, 60);
end;

procedure TPerson.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TPerson.Destroy;
begin
  inherited Destroy;
end;

{ TIndividualEntrepreneurInformation }

procedure TIndividualEntrepreneurInformation.SetINN(AValue: string);
begin
  if FINN=AValue then Exit;
  FINN:=AValue;
  ModifiedProperty('INN');
end;

procedure TIndividualEntrepreneurInformation.SetIndividualEntityRegistrationCertificate
  (AValue: string);
begin
  if FIndividualEntityRegistrationCertificate=AValue then Exit;
  FIndividualEntityRegistrationCertificate:=AValue;
  ModifiedProperty('IndividualEntityRegistrationCertificate');
end;

procedure TIndividualEntrepreneurInformation.SetINNDef(AValue: string);
begin
  if FINNDef=AValue then Exit;
  FINNDef:=AValue;
  ModifiedProperty('INNDef');
end;

procedure TIndividualEntrepreneurInformation.SetOtherInfo(AValue: string);
begin
  if FOtherInfo=AValue then Exit;
  FOtherInfo:=AValue;
  ModifiedProperty('OtherInfo');
end;

procedure TIndividualEntrepreneurInformation.InternalRegisterPropertys;
begin
  RegisterProperty('INN', 'ИННФЛ', 'Н', 'ИНН', 12, 12);
  RegisterProperty('INNDef', 'ИННФЛ', 'Н', 'ИНН при составлении документа с Функция=СЧФ или Функция=СЧФДОП налоговым агентом, указанным в пунктах 2 и 3 статьи 161 НК РФ, в части информации о продавце/грузоотправителе', 1, 1);
  RegisterProperty('IndividualEntityRegistrationCertificate', 'СвГосРегИП', 'Н', 'ИНН при составлении документа с Функция=СЧФ или Функция=СЧФДОП налоговым агентом, указанным в пунктах 2 и 3 статьи 161 НК РФ, в части информации о продавце/грузоотправителе', 1, 100);
  RegisterProperty('OtherInfo', 'СвГосРегИП', 'Н', 'Иные сведения, идентифицирующие физическое лицо', 1, 255);
  RegisterProperty('Person', 'ФИО', 'О', 'Фамилия, имя, отчество', -1, -1);
end;

procedure TIndividualEntrepreneurInformation.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FPerson:=TPerson.Create;
end;

destructor TIndividualEntrepreneurInformation.Destroy;
begin
  FreeAndNil(FPerson);
  inherited Destroy;
end;

{ TIdentificationInformation }

procedure TIdentificationInformation.InternalRegisterPropertys;
begin
  RegisterProperty('IndividualEntrepreneurInformation', 'СвИП', 'О', 'Сведения об индивидуальном предпринимателе', -1, -1);
  RegisterProperty('LegalEntityInformation', 'СвЮЛУч', 'О', 'Сведения о юридическом лице, состоящем на учете в налоговых органах', -1, -1);
  RegisterProperty('ForeignEntityInformation', 'СвИнНеУч', 'О', 'Сведения об иностранном лице, не состоящем на учете в налоговых органах в качестве налогоплательщика', -1, -1);
  RegisterProperty('PhysicalPerson', 'СвФЛУчастФХЖ', 'О', 'Сведения о физическом лице', -1, -1);
end;

procedure TIdentificationInformation.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FIndividualEntrepreneurInformation:=TIndividualEntrepreneurInformation.Create;
  FLegalEntityInformation:=TLegalEntityInformation.Create;
  FForeignEntityInformation:=TForeignEntityInformation.Create;
  FPhysicalPerson:=TPhysicalPersonEntity.Create;
end;

destructor TIdentificationInformation.Destroy;
begin
  FreeAndNil(FIndividualEntrepreneurInformation);
  FreeAndNil(FLegalEntityInformation);
  FreeAndNil(FForeignEntityInformation);
  FreeAndNil(FPhysicalPerson);
  inherited Destroy;
end;

{ TOrganizationInfoList }

function TOrganizationInfoList.GetItem(AIndex: Integer): TOrganizationInfo;
begin
  Result:=TOrganizationInfo(InternalGetItem(AIndex));
end;

constructor TOrganizationInfoList.Create;
begin
  inherited Create(TOrganizationInfo)
end;

{ TOrganizationInfo }

procedure TOrganizationInfo.SetOKPO(AValue: string);
begin
  if FOKPO=AValue then Exit;
  FOKPO:=AValue;
  ModifiedProperty('OKPO');
end;

procedure TOrganizationInfo.SetOrganizationOrPersonInfo(AValue: string);
begin
  if FOrganizationOrPersonInfo=AValue then Exit;
  FOrganizationOrPersonInfo:=AValue;
  ModifiedProperty('OrganizationOrPersonInfo');
end;

procedure TOrganizationInfo.SetShortName(AValue: string);
begin
  if FShortName=AValue then Exit;
  FShortName:=AValue;
  ModifiedProperty('ShortName');
end;

procedure TOrganizationInfo.SetDepartment(AValue: string);
begin
  if FDepartment=AValue then Exit;
  FDepartment:=AValue;
  ModifiedProperty('Department');
end;

procedure TOrganizationInfo.InternalRegisterPropertys;
begin
  RegisterProperty('OKPO', 'ОКПО', 'НК', 'Код в общероссийском классификаторе предприятий и организаций', 1, 10);
  RegisterProperty('Department', 'СтруктПодр', 'Н', 'Структурное подразделение', 1, 1000);
  RegisterProperty('OrganizationOrPersonInfo', 'ИнфДляУчаст', 'Н', 'Информация для участника документооборота', 1, 255);
  RegisterProperty('ShortName', 'КраткНазв', 'Н', 'Краткое название', 1, 255);
  RegisterProperty('IdentificationInformation', 'ИдСв', 'О', 'Идентификационные сведения', -1, -1);
  RegisterProperty('Adress', 'Адрес', 'Н', 'Адрес', -1, -1);
  RegisterProperty('ContactInfo', 'Контакт', 'Н', 'Контактные данные', -1, -1);
  RegisterProperty('BankDetails', 'БанкРекв', 'Н', 'Банковские реквизиты', -1, -1);
end;

procedure TOrganizationInfo.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FIdentificationInformation:=TIdentificationInformation.Create;
  FAdress:=TAdress.Create;
  FContactInfo:=TContactInfo.Create;
  FBankDetails:=TBankDetails.Create;
end;

destructor TOrganizationInfo.Destroy;
begin
  FreeAndNil(FIdentificationInformation);
  FreeAndNil(FAdress);
  FreeAndNil(FContactInfo);
  FreeAndNil(FBankDetails);
  inherited Destroy;
end;

end.

