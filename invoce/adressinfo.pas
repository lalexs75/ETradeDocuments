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

unit AdressInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlobject;

type

  { TRussianAdress }

  TRussianAdress =  class(TXmlSerializationObject)  //%Таблица 5.40
  private
    FApartment: string;
    FBlock: string;
    FBuilding: string;
    FCity: string;
    FLocality: string;
    FRegion: string;
    FStreet: string;
    FTerritory: string;
    FZipCode: string;
    procedure SetApartment(AValue: string);
    procedure SetBlock(AValue: string);
    procedure SetBuilding(AValue: string);
    procedure SetCity(AValue: string);
    procedure SetLocality(AValue: string);
    procedure SetRegion(AValue: string);
    procedure SetStreet(AValue: string);
    procedure SetTerritory(AValue: string);
    procedure SetZipCode(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property ZipCode:string read FZipCode write SetZipCode;
    property Region:string read FRegion write SetRegion;
    property Territory:string read FTerritory write SetTerritory;
    property City:string read FCity write SetCity;
    property Locality:string read FLocality write SetLocality;
    property Street:string read FStreet write SetStreet;
    property Building:string read FBuilding write SetBuilding;
    property Block:string read FBlock write SetBlock;
    property Apartment:string read FApartment write SetApartment;
  end;

  { TAdressInfo }

  TAdressInfo =  class(TXmlSerializationObject)  //%Таблица 5.41
  private
    FAddress: string;
    FCountryCode: string;
    procedure SetAddress(AValue: string);
    procedure SetCountryCode(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property CountryCode:string read FCountryCode write SetCountryCode;
    property Address:string read FAddress write SetAddress;
  end;

  { TAdress }

  TAdress =  class(TXmlSerializationObject)  //%Таблица 5.39
  private
    FAdressInfo: TAdressInfo;
    FCodeGAR: string;
    FRussianAdress: TRussianAdress;
    procedure SetCodeGAR(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property RussianAdress:TRussianAdress read FRussianAdress;
    property AdressInfo:TAdressInfo read FAdressInfo;
    property CodeGAR:string read FCodeGAR write SetCodeGAR;
  end;

implementation

{ TAdressInfo }

procedure TAdressInfo.SetAddress(AValue: string);
begin
  if FAddress=AValue then Exit;
  FAddress:=AValue;
  ModifiedProperty('Address');
end;

procedure TAdressInfo.SetCountryCode(AValue: string);
begin
  if FCountryCode=AValue then Exit;
  FCountryCode:=AValue;
  ModifiedProperty('CountryCode');
end;

procedure TAdressInfo.InternalRegisterPropertys;
begin
  RegisterProperty('CountryCode', 'КодСтр', 'ОК', 'Код страны', 3, 3);
  RegisterProperty('Address', 'АдрТекст', 'О', 'Адрес', 1, 1000);
end;

procedure TAdressInfo.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TAdressInfo.Destroy;
begin
  inherited Destroy;
end;

{ TRussianAdress }

procedure TRussianAdress.SetApartment(AValue: string);
begin
  if FApartment=AValue then Exit;
  FApartment:=AValue;
  ModifiedProperty('Apartment');
end;

procedure TRussianAdress.SetBlock(AValue: string);
begin
  if FBlock=AValue then Exit;
  FBlock:=AValue;
  ModifiedProperty('Block');
end;

procedure TRussianAdress.SetBuilding(AValue: string);
begin
  if FBuilding=AValue then Exit;
  FBuilding:=AValue;
  ModifiedProperty('Building');
end;

procedure TRussianAdress.SetCity(AValue: string);
begin
  if FCity=AValue then Exit;
  FCity:=AValue;
  ModifiedProperty('City');
end;

procedure TRussianAdress.SetLocality(AValue: string);
begin
  if FLocality=AValue then Exit;
  FLocality:=AValue;
  ModifiedProperty('Locality');
end;

procedure TRussianAdress.SetRegion(AValue: string);
begin
  if FRegion=AValue then Exit;
  FRegion:=AValue;
  ModifiedProperty('Region');
end;

procedure TRussianAdress.SetStreet(AValue: string);
begin
  if FStreet=AValue then Exit;
  FStreet:=AValue;
  ModifiedProperty('Street');
end;

procedure TRussianAdress.SetTerritory(AValue: string);
begin
  if FTerritory=AValue then Exit;
  FTerritory:=AValue;
  ModifiedProperty('Territory');
end;

procedure TRussianAdress.SetZipCode(AValue: string);
begin
  if FZipCode=AValue then Exit;
  FZipCode:=AValue;
  ModifiedProperty('ZipCode');
end;

procedure TRussianAdress.InternalRegisterPropertys;
begin
  RegisterProperty('ZipCode', 'Индекс', 'Н', 'Индекс', 6, 6);
  RegisterProperty('Region', 'КодРегион', 'Н', 'Код региона', 2, 2);
  RegisterProperty('Territory', 'Район', 'Н', 'Район', 1, 50);
  RegisterProperty('City', 'Город', 'Н', 'Город', 1, 50);
  RegisterProperty('Locality', 'НаселПункт', 'Н', 'Населенный пункт', 1, 50);
  RegisterProperty('Street', 'Улица', 'Н', 'Улица', 1, 50);
  RegisterProperty('Building', 'Дом', 'Н', 'Дом', 1, 20);
  RegisterProperty('Block', 'Корпус', 'Н', 'Корпус', 1, 20);
  RegisterProperty('Apartment', 'Кварт', 'Н', 'Квартира', 1, 20);
end;

procedure TRussianAdress.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TRussianAdress.Destroy;
begin
  inherited Destroy;
end;

{ TAdress }

procedure TAdress.SetCodeGAR(AValue: string);
begin
  if FCodeGAR=AValue then Exit;
  FCodeGAR:=AValue;
  ModifiedProperty(CodeGAR);
end;

procedure TAdress.InternalRegisterPropertys;
begin
  RegisterProperty('RussianAdress', 'АдрРФ', 'О', 'Адрес, указанный в Едином государственном реестре юридических лиц/почтовый адрес/адрес места жительства индивидуального предпринимателя (реквизиты адреса на территории Российской Федерации)', -1, -1);
  RegisterProperty('AdressInfo', 'АдрИнф', 'О', 'Адрес, указанный в Едином государственном реестре юридических лиц/почтовый адрес/адрес места жительства индивидуального предпринимателя (информация об адресе, в том числе об адресе за пределами территории Российской Федерации)', -1, -1);
(*
Уникальный номер адреса объекта адресации в государственном адресном реестре 	КодГАР 	П 	T(1-36) 	О

*)
end;

procedure TAdress.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FRussianAdress:=TRussianAdress.Create;
  FAdressInfo:=TAdressInfo.Create;
end;

destructor TAdress.Destroy;
begin
  FreeAndNil(FRussianAdress);
  FreeAndNil(FAdressInfo);
  inherited Destroy;
end;

end.

