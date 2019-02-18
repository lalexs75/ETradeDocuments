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
  Classes, SysUtils, xml_doc;

type

  { TExchangePhysicalPersonEntity }

  TExchangePhysicalPersonEntity = class(TXmlSerializationObject)   //%Таблица 7.21
  private
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
  end;

  { TExchangeIndividualEntrepreneurInformation }

  TExchangeIndividualEntrepreneurInformation = class(TXmlSerializationObject)   //%Таблица 7.20
  private
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
  end;


  { TExchangeLegalEntityInformation }

  TExchangeLegalEntityInformation = class(TXmlSerializationObject) //%Таблица 7.19
  private
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
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
    property Item[AIndex:Integer]:TExchangeSigner read GetItem;
  end;

implementation

{ TExchangeIndividualEntrepreneurInformation }

procedure TExchangeIndividualEntrepreneurInformation.InternalRegisterPropertys;
begin

end;

procedure TExchangeIndividualEntrepreneurInformation.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TExchangeIndividualEntrepreneurInformation.Destroy;
begin
  inherited Destroy;
end;

{ TExchangePhysicalPersonEntity }

procedure TExchangePhysicalPersonEntity.InternalRegisterPropertys;
begin

end;

procedure TExchangePhysicalPersonEntity.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TExchangePhysicalPersonEntity.Destroy;
begin
  inherited Destroy;
end;

{ TExchangeLegalEntityInformation }

procedure TExchangeLegalEntityInformation.InternalRegisterPropertys;
begin
  (*
  ИНН организации 	ИННЮЛ 	А 	T(=10) 	О 	Типовой элемент <ИННЮЛТип>
  Наименование 	НаимОрг 	А 	T(1-1000) 	Н
  Должность 	Должн 	А 	T(1-128) 	О
  Иные сведения, идентифицирующие физическое лицо 	ИныеСвед 	А 	T(1-255) 	Н
  Фамилия, имя, отчество 	ФИО 	С 		О 	Типовой элемент <ФИОТип>.
  Состав элемента представлен в таблице 7.22
  *)
end;

procedure TExchangeLegalEntityInformation.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TExchangeLegalEntityInformation.Destroy;
begin
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

end.

