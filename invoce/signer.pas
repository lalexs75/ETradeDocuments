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

unit Signer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xml_doc, OrganizationInfo;

type

  { TSigner }

  TSigner = class(TXmlSerializationObject) //%Таблица 5.31
  private
    FIndividualEntrepreneurInformation: TIndividualEntrepreneurInformation;
    FLegalEntityEmployee: TLegalEntityEmployee;
    FPhysicalPersonEntity: TPhysicalPersonEntity;
    FSignerOrgPowersBase: string;
    FSignerPowers: string;
    FSignerPowersBase: string;
    FSignerStatus: string;
    procedure SetSignerOrgPowersBase(AValue: string);
    procedure SetSignerPowers(AValue: string);
    procedure SetSignerPowersBase(AValue: string);
    procedure SetSignerStatus(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property SignerPowers:string read FSignerPowers write SetSignerPowers;
    property SignerStatus:string read FSignerStatus write SetSignerStatus;
    property SignerPowersBase:string read FSignerPowersBase write SetSignerPowersBase;
    property SignerOrgPowersBase:string read FSignerOrgPowersBase write SetSignerOrgPowersBase;
    property PhysicalPersonEntity:TPhysicalPersonEntity read FPhysicalPersonEntity;
    property IndividualEntrepreneurInformation:TIndividualEntrepreneurInformation read FIndividualEntrepreneurInformation;
    property LegalEntityEmployee:TLegalEntityEmployee read FLegalEntityEmployee;
  end;

  { TSignerList }

  TSignerList = class(TXmlSerializationObjectList) //%Таблица 5.31
  private
    function GetItem(AIndex: Integer): TSigner; inline;
  public
    constructor Create;
    property Item[AIndex:Integer]:TSigner read GetItem; default;
  end;

implementation

{ TSigner }

procedure TSigner.SetSignerOrgPowersBase(AValue: string);
begin
  if FSignerOrgPowersBase=AValue then Exit;
  FSignerOrgPowersBase:=AValue;
  ModifiedProperty('SignerOrgPowersBase');
end;

procedure TSigner.SetSignerPowers(AValue: string);
begin
  if FSignerPowers=AValue then Exit;
  FSignerPowers:=AValue;
  ModifiedProperty('SignerPowers');
end;

procedure TSigner.SetSignerPowersBase(AValue: string);
begin
  if FSignerPowersBase=AValue then Exit;
  FSignerPowersBase:=AValue;
  ModifiedProperty('SignerPowersBase');
end;

procedure TSigner.SetSignerStatus(AValue: string);
begin
  if FSignerStatus=AValue then Exit;
  FSignerStatus:=AValue;
  ModifiedProperty('SignerStatus');
end;

procedure TSigner.InternalRegisterPropertys;
begin
  RegisterProperty('SignerPowers', 'ОблПолн', 'ОК', 'Область полномочий', 1, 2);
  RegisterProperty('SignerStatus', 'Статус', 'ОК', 'Статус', 1, 2);
  RegisterProperty('SignerPowersBase', 'ОснПолн', 'О', 'Основание полномочий (доверия)', 1, 255);
  RegisterProperty('SignerOrgPowersBase', 'ОснПолнОрг', 'Н', 'Основание полномочий (доверия) организации', 1, 255);
  RegisterProperty('PhysicalPersonEntity', 'ФЛ', 'О', 'Физическое лицо', -1, -1);
  RegisterProperty('IndividualEntrepreneurInformation', 'ИП', 'О', 'Индивидуальный предприниматель', -1, -1);
  RegisterProperty('LegalEntityEmployee', 'ЮЛ', 'О', 'Представитель юридического лица', -1, -1);
end;

procedure TSigner.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FPhysicalPersonEntity:=TPhysicalPersonEntity.Create;
  FIndividualEntrepreneurInformation:=TIndividualEntrepreneurInformation.Create;
  FLegalEntityEmployee:=TLegalEntityEmployee.Create;
end;

destructor TSigner.Destroy;
begin
  FreeAndNil(FPhysicalPersonEntity);
  FreeAndNil(FIndividualEntrepreneurInformation);
  FreeAndNil(FLegalEntityEmployee);
  inherited Destroy;
end;

{ TSignerList }

function TSignerList.GetItem(AIndex: Integer): TSigner;
begin
  Result:=TSigner(InternalGetItem(AIndex));
end;

constructor TSignerList.Create;
begin
  inherited Create(TSigner)
end;

end.

