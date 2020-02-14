{ GS1 interface library for FPC and Lazarus

  Copyright (C) 2020 Lagunov Aleksey alexs75@yandex.ru

  base on docs from http://api-docs.diadoc.ru

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

unit Vvod;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlobject;

type
  TDocType = String;
  TParticipant_INN = String;
  TProduct_Date = TDate;
  TProducer_INN = String;
  TINNOwner = String;
  TProduction_Order = Longint;
  TMarkingType = Longint;
  TProduct_Date1 = TDate;
  TUIT = String;
  TUITU = String;
  TCodeTNVED = String;
  TMarkingType1 = Longint;
  TCertificateDoc = Longint;
  TCertificateNumber = String;
  TCertificateDate = TDate;
type

  {  Forward declarations  }
  TVvod = class;
  TVvod_element = class;
  TVvod_Products_List = class;
  TVvod_Products_List_PRODUCT = class;

  {  Generic classes for collections  }
  TVvodList = specialize GXMLSerializationObjectList<TVvod>;
  TVvod_elementList = specialize GXMLSerializationObjectList<TVvod_element>;
  TVvod_Products_ListList = specialize GXMLSerializationObjectList<TVvod_Products_List>;
  TVvod_Products_List_PRODUCTList = specialize GXMLSerializationObjectList<TVvod_Products_List_PRODUCT>;

  {  TVvod  }
  //Заявка на ввод товаров в оборот (собственное производство)
  TVvod = class(TXmlSerializationObject)
  private
    FDocType:TDocType;
    FParticipant_INN:TParticipant_INN;
    FProduct_Date:TProduct_Date;
    FProducer_INN:TProducer_INN;
    FINNOwner:TINNOwner;
    FProduction_Order:TProduction_Order;
    FMarkingType:TMarkingType;
    FProducts_List:TVvod_Products_List;
    Faction_id:Longint;
    procedure SetDocType( AValue:TDocType);
    procedure SetParticipant_INN( AValue:TParticipant_INN);
    procedure SetProduct_Date( AValue:TProduct_Date);
    procedure SetProducer_INN( AValue:TProducer_INN);
    procedure SetINNOwner( AValue:TINNOwner);
    procedure SetProduction_Order( AValue:TProduction_Order);
    procedure SetMarkingType( AValue:TMarkingType);
    procedure Setaction_id( AValue:Longint);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property DocType:TDocType read FDocType write SetDocType;
    property Participant_INN:TParticipant_INN read FParticipant_INN write SetParticipant_INN;
    property Product_Date:TProduct_Date read FProduct_Date write SetProduct_Date;
    property Producer_INN:TProducer_INN read FProducer_INN write SetProducer_INN;
    property INNOwner:TINNOwner read FINNOwner write SetINNOwner;
    //Production_Order
    //1 - Собственное производство
    //2 - Производство товара по договору
    property Production_Order:TProduction_Order read FProduction_Order write SetProduction_Order;
    //Вид маркировки
    //1 - На потребительскую упаковку
    //2 - На товар
    //3 - На товарный ярлык
    property MarkingType:TMarkingType read FMarkingType write SetMarkingType;
    property Products_List:TVvod_Products_List read FProducts_List;
    property action_id:Longint read Faction_id write Setaction_id;
  end;

  {  TVvod_element  }
  TVvod_element = class(TVvod)
  private
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
    function RootNodeName:string; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
  end;

  {  TVvod_Products_List  }
  TVvod_Products_List = class(TXmlSerializationObject)
  private
    FPRODUCT:TVvod_Products_List_PRODUCTList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property PRODUCT:TVvod_Products_List_PRODUCTList read FPRODUCT;
  end;

  {  TVvod_Products_List_PRODUCT  }
  TVvod_Products_List_PRODUCT = class(TXmlSerializationObject)
  private
    FProduct_Date:TProduct_Date1;
    FUIT:TUIT;
    FUITU:TUITU;
    FCodeTNVED:TCodeTNVED;
    FMarkingType:TMarkingType1;
    FCertificateDoc:TCertificateDoc;
    FCertificateNumber:TCertificateNumber;
    FCertificateDate:TCertificateDate;
    procedure SetProduct_Date( AValue:TProduct_Date1);
    procedure SetUIT( AValue:TUIT);
    procedure SetUITU( AValue:TUITU);
    procedure SetCodeTNVED( AValue:TCodeTNVED);
    procedure SetMarkingType( AValue:TMarkingType1);
    procedure SetCertificateDoc( AValue:TCertificateDoc);
    procedure SetCertificateNumber( AValue:TCertificateNumber);
    procedure SetCertificateDate( AValue:TCertificateDate);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Product_Date:TProduct_Date1 read FProduct_Date write SetProduct_Date;
    property UIT:TUIT read FUIT write SetUIT;
    property UITU:TUITU read FUITU write SetUITU;
    property CodeTNVED:TCodeTNVED read FCodeTNVED write SetCodeTNVED;
    //Вид маркировки
    //1 - На потребительскую упаковку
    //2 - На товар
    //3 - На товарный ярлык
    property MarkingType:TMarkingType1 read FMarkingType write SetMarkingType;
    //Документ Обязательной Сертификации
    //1 - Сертификат соответствия
    //2 - Декларация соответствия
    //3 - Отказное письмо
    property CertificateDoc:TCertificateDoc read FCertificateDoc write SetCertificateDoc;
    property CertificateNumber:TCertificateNumber read FCertificateNumber write SetCertificateNumber;
    property CertificateDate:TCertificateDate read FCertificateDate write SetCertificateDate;
  end;

implementation

  {  TVvod  }
procedure TVvod.SetDocType(AValue: TDocType);
begin
  FDocType:=AValue;
  CheckLockupValue('DocType', AValue);
  ModifiedProperty('DocType');
end;

procedure TVvod.SetParticipant_INN(AValue: TParticipant_INN);
begin
  FParticipant_INN:=AValue;
  CheckStrMinSize('Participant_INN', AValue);
  CheckStrMaxSize('Participant_INN', AValue);
  ModifiedProperty('Participant_INN');
end;

procedure TVvod.SetProduct_Date(AValue: TProduct_Date);
begin
  FProduct_Date:=AValue;
  ModifiedProperty('Product_Date');
end;

procedure TVvod.SetProducer_INN(AValue: TProducer_INN);
begin
  FProducer_INN:=AValue;
  CheckStrMinSize('Producer_INN', AValue);
  CheckStrMaxSize('Producer_INN', AValue);
  ModifiedProperty('Producer_INN');
end;

procedure TVvod.SetINNOwner(AValue: TINNOwner);
begin
  FINNOwner:=AValue;
  CheckStrMinSize('INNOwner', AValue);
  CheckStrMaxSize('INNOwner', AValue);
  ModifiedProperty('INNOwner');
end;

procedure TVvod.SetProduction_Order(AValue: TProduction_Order);
begin
  FProduction_Order:=AValue;
  CheckLockupValue('Production_Order', AValue);
  ModifiedProperty('Production_Order');
end;

procedure TVvod.SetMarkingType(AValue: TMarkingType);
begin
  FMarkingType:=AValue;
  CheckLockupValue('MarkingType', AValue);
  ModifiedProperty('MarkingType');
end;

procedure TVvod.Setaction_id(AValue: Longint);
begin
  Faction_id:=AValue;
  CheckFixedValue('action_id', AValue);
  ModifiedProperty('action_id');
end;

procedure TVvod.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('DocType', 'DocType', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('Promotion_Inform_Selfmade');
  P:=RegisterProperty('Participant_INN', 'Participant_INN', [xsaSimpleObject], '', 10, 12);
  P:=RegisterProperty('Product_Date', 'Product_Date', [xsaSimpleObject], '', -1, -1);
  P:=RegisterProperty('Producer_INN', 'Producer_INN', [xsaSimpleObject], '', 10, 12);
  P:=RegisterProperty('INNOwner', 'INNOwner', [xsaSimpleObject], '', 10, 12);
  P:=RegisterProperty('Production_Order', 'Production_Order', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('1');
    P.ValidList.Add('2');
  P:=RegisterProperty('MarkingType', 'MarkingType', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('1');
    P.ValidList.Add('2');
    P.ValidList.Add('3');
  P:=RegisterProperty('Products_List', 'Products_List', [], '', -1, -1);
  P:=RegisterProperty('action_id', 'action_id', [xsaRequared], '', -1, -1);
    P.DefaultValue:='05';
end;

procedure TVvod.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FProducts_List:=TVvod_Products_List.Create;
end;

destructor TVvod.Destroy;
begin
  FProducts_List.Free;
  inherited Destroy;
end;

constructor TVvod.Create;
begin
  inherited Create;
  action_id:=05;
end;

  {  TVvod_element  }
procedure TVvod_element.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
end;

procedure TVvod_element.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TVvod_element.Destroy;
begin
  inherited Destroy;
end;

function TVvod_element.RootNodeName:string;
begin
  Result:='Vvod';
end;

constructor TVvod_element.Create;
begin
  inherited Create;
end;

  {  TVvod_Products_List  }
procedure TVvod_Products_List.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('PRODUCT', 'PRODUCT', [], '', -1, -1);
end;

procedure TVvod_Products_List.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FPRODUCT:=TVvod_Products_List_PRODUCTList.Create;
end;

destructor TVvod_Products_List.Destroy;
begin
  FPRODUCT.Free;
  inherited Destroy;
end;

constructor TVvod_Products_List.Create;
begin
  inherited Create;
end;

  {  TVvod_Products_List_PRODUCT  }
procedure TVvod_Products_List_PRODUCT.SetProduct_Date(AValue: TProduct_Date1);
begin
  FProduct_Date:=AValue;
  ModifiedProperty('Product_Date');
end;

procedure TVvod_Products_List_PRODUCT.SetUIT(AValue: TUIT);
begin
  FUIT:=AValue;
  CheckStrMinSize('UIT', AValue);
  CheckStrMaxSize('UIT', AValue);
  ModifiedProperty('UIT');
end;

procedure TVvod_Products_List_PRODUCT.SetUITU(AValue: TUITU);
begin
  FUITU:=AValue;
  CheckStrMinSize('UITU', AValue);
  CheckStrMaxSize('UITU', AValue);
  ModifiedProperty('UITU');
end;

procedure TVvod_Products_List_PRODUCT.SetCodeTNVED(AValue: TCodeTNVED);
begin
  FCodeTNVED:=AValue;
  CheckStrMinSize('CodeTNVED', AValue);
  CheckStrMaxSize('CodeTNVED', AValue);
  ModifiedProperty('CodeTNVED');
end;

procedure TVvod_Products_List_PRODUCT.SetMarkingType(AValue: TMarkingType1);
begin
  FMarkingType:=AValue;
  CheckLockupValue('MarkingType', AValue);
  ModifiedProperty('MarkingType');
end;

procedure TVvod_Products_List_PRODUCT.SetCertificateDoc(AValue: TCertificateDoc);
begin
  FCertificateDoc:=AValue;
  CheckLockupValue('CertificateDoc', AValue);
  ModifiedProperty('CertificateDoc');
end;

procedure TVvod_Products_List_PRODUCT.SetCertificateNumber(AValue: TCertificateNumber);
begin
  FCertificateNumber:=AValue;
  ModifiedProperty('CertificateNumber');
end;

procedure TVvod_Products_List_PRODUCT.SetCertificateDate(AValue: TCertificateDate);
begin
  FCertificateDate:=AValue;
  ModifiedProperty('CertificateDate');
end;

procedure TVvod_Products_List_PRODUCT.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('Product_Date', 'Product_Date', [xsaSimpleObject], '', -1, -1);
  P:=RegisterProperty('UIT', 'UIT', [xsaSimpleObject], '', 31, 38);
  P:=RegisterProperty('UITU', 'UITU', [xsaSimpleObject], '', 18, 18);
  P:=RegisterProperty('CodeTNVED', 'CodeTNVED', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('MarkingType', 'MarkingType', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('1');
    P.ValidList.Add('2');
    P.ValidList.Add('3');
  P:=RegisterProperty('CertificateDoc', 'CertificateDoc', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('1');
    P.ValidList.Add('2');
    P.ValidList.Add('3');
  P:=RegisterProperty('CertificateNumber', 'CertificateNumber', [xsaSimpleObject], '', -1, -1);
  P:=RegisterProperty('CertificateDate', 'CertificateDate', [xsaSimpleObject], '', -1, -1);
end;

procedure TVvod_Products_List_PRODUCT.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TVvod_Products_List_PRODUCT.Destroy;
begin
  inherited Destroy;
end;

constructor TVvod_Products_List_PRODUCT.Create;
begin
  inherited Create;
end;

end.