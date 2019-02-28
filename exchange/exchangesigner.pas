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

unit ExchangeSigner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xml_doc, ExchangeInformation;

type

  { TExchangePhysicalPersonEntity }

  TExchangePhysicalPersonEntity = class(TXmlSerializationObject)   //%Таблица 7.21
  private
    FInn: string;
    FOtherInfo: string;
    FPerson: TExchangePerson;
    procedure SetInn(AValue: string);
    procedure SetOtherInfo(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property Inn:string read FInn write SetInn;
    property OtherInfo:string read FOtherInfo write SetOtherInfo;
    property Person:TExchangePerson read FPerson;
  end;

  { TExchangeIndividualEntrepreneurInformation }

  TExchangeIndividualEntrepreneurInformation = class(TXmlSerializationObject)   //%Таблица 7.20
  private
    FIndividualEntityRegistrationCertificate: string;
    FInn: string;
    FOtherInfo: string;
    FPerson: TExchangePerson;
    procedure SetIndividualEntityRegistrationCertificate(AValue: string);
    procedure SetInn(AValue: string);
    procedure SetOtherInfo(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property Inn:string read FInn write SetInn;
    property IndividualEntityRegistrationCertificate:string read FIndividualEntityRegistrationCertificate write SetIndividualEntityRegistrationCertificate;
    property OtherInfo:string read FOtherInfo write SetOtherInfo;
    property Person:TExchangePerson read FPerson;
  end;


  { TExchangeLegalEntityInformation }

  TExchangeLegalEntityInformation = class(TXmlSerializationObject) //%Таблица 7.19
  private
    FInn: string;
    FName: string;
    FOtherInfo: string;
    FPerson: TExchangePerson;
    FPosition: string;
    procedure SetInn(AValue: string);
    procedure SetName(AValue: string);
    procedure SetOtherInfo(AValue: string);
    procedure SetPosition(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property Inn:string read FInn write SetInn;
    property Name:string read FName write SetName;
    property Position:string read FPosition write SetPosition;
    property OtherInfo:string read FOtherInfo write SetOtherInfo;
    property Person:TExchangePerson read FPerson;
  end;

  { TExchangeSigner }

  TExchangeSigner = class(TXmlSerializationObject)   //%Таблица 7.18
  private
    FIndividualEntrepreneurInformation: TExchangeIndividualEntrepreneurInformation;
    FLegalEntityInformation: TExchangeLegalEntityInformation;
    FPhysicalPersonEntity: TExchangePhysicalPersonEntity;
    FSignerOrgPowersBase: string;
    FSignerPowers: string;
    FSignerPowersBase: string;
    FStatus: string;
    procedure SetSignerOrgPowersBase(AValue: string);
    procedure SetSignerPowers(AValue: string);
    procedure SetSignerPowersBase(AValue: string);
    procedure SetStatus(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property SignerPowers:string read FSignerPowers write SetSignerPowers;
    property Status:string read FStatus write SetStatus;
    property SignerPowersBase:string read FSignerPowersBase write SetSignerPowersBase;
    property SignerOrgPowersBase:string read FSignerOrgPowersBase write SetSignerOrgPowersBase;
    property PhysicalPersonEntity:TExchangePhysicalPersonEntity read FPhysicalPersonEntity;
    property IndividualEntrepreneurInformation:TExchangeIndividualEntrepreneurInformation read FIndividualEntrepreneurInformation;
    property LegalEntityInformation:TExchangeLegalEntityInformation read FLegalEntityInformation;
  end;

  { TExchangeSignerList }

  TExchangeSignerList = class(TXmlSerializationObjectList) //%Таблица 7.18
  private
    function GetItem(AIndex: Integer): TExchangeSigner; inline;
  public
    constructor Create;
    function CreateChild:TExchangeSigner;
    property Item[AIndex:Integer]:TExchangeSigner read GetItem; default;
  end;

implementation

{ TExchangeIndividualEntrepreneurInformation }

procedure TExchangeIndividualEntrepreneurInformation.SetIndividualEntityRegistrationCertificate
  (AValue: string);
begin
  if FIndividualEntityRegistrationCertificate=AValue then Exit;
  FIndividualEntityRegistrationCertificate:=AValue;
  ModifiedProperty('IndividualEntityRegistrationCertificate');
end;

procedure TExchangeIndividualEntrepreneurInformation.SetInn(AValue: string);
begin
  if FInn=AValue then Exit;
  FInn:=AValue;
  ModifiedProperty('Inn');
end;

procedure TExchangeIndividualEntrepreneurInformation.SetOtherInfo(AValue: string
  );
begin
  if FOtherInfo=AValue then Exit;
  FOtherInfo:=AValue;
  ModifiedProperty('OtherInfo');
end;

procedure TExchangeIndividualEntrepreneurInformation.InternalRegisterPropertys;
begin
  RegisterProperty('Inn', 'ИННФЛ', 'О', 'ИНН', 12, 12);
  RegisterProperty('IndividualEntityRegistrationCertificate', 'СвГосРегИП', 'Н', 'Реквизиты свидетельства о государственной регистрации индивидуального предпринимателя', 1, 100);
  RegisterProperty('OtherInfo', 'ИныеСвед', 'Н', 'Иные сведения, идентифицирующие физическое лицо', 1, 255);
  RegisterProperty('Person', 'ФИО', 'О', 'Фамилия, имя, отчество', -1, -1);
end;

procedure TExchangeIndividualEntrepreneurInformation.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FPerson:=TExchangePerson.Create;
end;

destructor TExchangeIndividualEntrepreneurInformation.Destroy;
begin
  FreeAndNil(FPerson);
  inherited Destroy;
end;

{ TExchangePhysicalPersonEntity }

procedure TExchangePhysicalPersonEntity.SetInn(AValue: string);
begin
  if FInn=AValue then Exit;
  FInn:=AValue;
  ModifiedProperty('Inn');
end;

procedure TExchangePhysicalPersonEntity.SetOtherInfo(AValue: string);
begin
  if FOtherInfo=AValue then Exit;
  FOtherInfo:=AValue;
  ModifiedProperty('OtherInfo');
end;

procedure TExchangePhysicalPersonEntity.InternalRegisterPropertys;
begin
  RegisterProperty('Inn', 'ИННФЛ', 'Н', 'ИНН', 12, 12);
  RegisterProperty('OtherInfo', 'ИныеСвед', 'Н', 'Иные сведения, идентифицирующие физическое лицо', 1, 255);
  RegisterProperty('Person', 'ФИО', 'О', 'Фамилия, имя, отчество', -1, -1);
end;

procedure TExchangePhysicalPersonEntity.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FPerson:=TExchangePerson.Create;
end;

destructor TExchangePhysicalPersonEntity.Destroy;
begin
  FreeAndNil(FPerson);
  inherited Destroy;
end;

{ TExchangeLegalEntityInformation }

procedure TExchangeLegalEntityInformation.SetInn(AValue: string);
begin
  if FInn=AValue then Exit;
  FInn:=AValue;
  ModifiedProperty('Inn');
end;

procedure TExchangeLegalEntityInformation.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
  ModifiedProperty('Name');
end;

procedure TExchangeLegalEntityInformation.SetOtherInfo(AValue: string);
begin
  if FOtherInfo=AValue then Exit;
  FOtherInfo:=AValue;
  ModifiedProperty('OtherInfo');
end;

procedure TExchangeLegalEntityInformation.SetPosition(AValue: string);
begin
  if FPosition=AValue then Exit;
  FPosition:=AValue;
  ModifiedProperty('Position');
end;

procedure TExchangeLegalEntityInformation.InternalRegisterPropertys;
begin
  RegisterProperty('Inn', 'ИННЮЛ', 'О', 'ИНН организации', 10, 10);
  RegisterProperty('Name', 'НаимОрг', 'Н', 'Наименование', 1, 1000);
  RegisterProperty('Position', 'Должн', 'О', 'Должность', 1, 128);
  RegisterProperty('OtherInfo', 'ИныеСвед', 'Н', 'Иные сведения, идентифицирующие физическое лицо', 1, 255);
  RegisterProperty('Person', 'ФИО', 'О', 'Фамилия, имя, отчество', -1, -1);
end;

procedure TExchangeLegalEntityInformation.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FPerson:=TExchangePerson.Create;
end;

destructor TExchangeLegalEntityInformation.Destroy;
begin
  FreeAndNil(FPerson);
  inherited Destroy;
end;

{ TExchangeSigner }

procedure TExchangeSigner.SetSignerOrgPowersBase(AValue: string);
begin
  if FSignerOrgPowersBase=AValue then Exit;
  FSignerOrgPowersBase:=AValue;
  ModifiedProperty('SignerOrgPowersBase');
end;

procedure TExchangeSigner.SetSignerPowers(AValue: string);
begin
  if FSignerPowers=AValue then Exit;
  FSignerPowers:=AValue;
  ModifiedProperty('SignerPowers');
end;

procedure TExchangeSigner.SetSignerPowersBase(AValue: string);
begin
  if FSignerPowersBase=AValue then Exit;
  FSignerPowersBase:=AValue;
  ModifiedProperty('SignerPowersBase');
end;

procedure TExchangeSigner.SetStatus(AValue: string);
begin
  if FStatus=AValue then Exit;
  FStatus:=AValue;
  ModifiedProperty('Status');
end;

procedure TExchangeSigner.InternalRegisterPropertys;
begin
  RegisterProperty('SignerPowers', 'ОблПолн', 'ОК', 'Область полномочий', 1, 1);
  RegisterProperty('Status', 'Статус', 'ОК', 'Статус', 1, 1);
  RegisterProperty('SignerPowersBase', 'ОснПолн', 'О', 'Основание полномочий (доверия)', 1, 255);
  RegisterProperty('SignerOrgPowersBase', 'ОснПолнОрг', 'Н', 'Основание полномочий (доверия) организации', 1, 255);
  RegisterProperty('PhysicalPersonEntity', 'ФЛ', 'О', 'Физическое лицо', -1, -1);
  RegisterProperty('IndividualEntrepreneurInformation', 'ИП', 'О', 'Индивидуальный предприниматель', -1, -1);
  RegisterProperty('LegalEntityInformation', 'ЮЛ', 'О', 'Представитель юридического лица', -1, -1);
end;

procedure TExchangeSigner.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FPhysicalPersonEntity:=TExchangePhysicalPersonEntity.Create;
  FIndividualEntrepreneurInformation:=TExchangeIndividualEntrepreneurInformation.Create;
  FLegalEntityInformation:=TExchangeLegalEntityInformation.Create;
end;

destructor TExchangeSigner.Destroy;
begin
  FreeAndNil(FPhysicalPersonEntity);
  FreeAndNil(FIndividualEntrepreneurInformation);
  FreeAndNil(FLegalEntityInformation);
  inherited Destroy;
end;

{ TExchangeSignerList }

function TExchangeSignerList.GetItem(AIndex: Integer): TExchangeSigner;
begin
  Result:=TExchangeSigner(InternalGetItem(AIndex));
end;

constructor TExchangeSignerList.Create;
begin
  inherited Create(TExchangeSigner)
end;

function TExchangeSignerList.CreateChild: TExchangeSigner;
begin
  Result:=InternalAddObject as TExchangeSigner;
end;

end.

