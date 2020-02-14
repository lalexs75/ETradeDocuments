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

unit vvod_ostatky;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlobject, LP_base_types_v3;

type
  Taoguid = String;
  Thouseguid = String;
  Tflat = String;
type

  {  Forward declarations  }
  Tfias_address_type = class;
  Tshipment_children_products_list_type = class;
  Tshipment_children_products_list_type_product = class;
  Tacceptance_children_products_list_type = class;
  Tacceptance_children_products_list_type_product = class;
  Tvvod_children_products_list_type = class;
  Tvvod_children_products_list_type_product = class;
  Tvvod_ind_children_products_list_type = class;
  Tvvod_ind_children_products_list_type_product = class;
  Tvvod_ost_children_products_list_type = class;
  Tvvod_ost_children_products_list_type_product = class;
  Tvvod_crossborder_products_list_type = class;
  Tvvod_crossborder_products_list_type_product = class;
  Tvvod_ostatky = class;
  Tvvod_ostatky_element = class;
  Tvvod_ostatky_products_list = class;
  Tvvod_ostatky_products_list_product = class;

  {  Generic classes for collections  }
  Tfias_address_typeList = specialize GXMLSerializationObjectList<Tfias_address_type>;
  Tshipment_children_products_list_typeList = specialize GXMLSerializationObjectList<Tshipment_children_products_list_type>;
  Tshipment_children_products_list_type_productList = specialize GXMLSerializationObjectList<Tshipment_children_products_list_type_product>;
  Tacceptance_children_products_list_typeList = specialize GXMLSerializationObjectList<Tacceptance_children_products_list_type>;
  Tacceptance_children_products_list_type_productList = specialize GXMLSerializationObjectList<Tacceptance_children_products_list_type_product>;
  Tvvod_children_products_list_typeList = specialize GXMLSerializationObjectList<Tvvod_children_products_list_type>;
  Tvvod_children_products_list_type_productList = specialize GXMLSerializationObjectList<Tvvod_children_products_list_type_product>;
  Tvvod_ind_children_products_list_typeList = specialize GXMLSerializationObjectList<Tvvod_ind_children_products_list_type>;
  Tvvod_ind_children_products_list_type_productList = specialize GXMLSerializationObjectList<Tvvod_ind_children_products_list_type_product>;
  Tvvod_ost_children_products_list_typeList = specialize GXMLSerializationObjectList<Tvvod_ost_children_products_list_type>;
  Tvvod_ost_children_products_list_type_productList = specialize GXMLSerializationObjectList<Tvvod_ost_children_products_list_type_product>;
  Tvvod_crossborder_products_list_typeList = specialize GXMLSerializationObjectList<Tvvod_crossborder_products_list_type>;
  Tvvod_crossborder_products_list_type_productList = specialize GXMLSerializationObjectList<Tvvod_crossborder_products_list_type_product>;
  Tvvod_ostatkyList = specialize GXMLSerializationObjectList<Tvvod_ostatky>;
  Tvvod_ostatky_elementList = specialize GXMLSerializationObjectList<Tvvod_ostatky_element>;
  Tvvod_ostatky_products_listList = specialize GXMLSerializationObjectList<Tvvod_ostatky_products_list>;
  Tvvod_ostatky_products_list_productList = specialize GXMLSerializationObjectList<Tvvod_ostatky_products_list_product>;

  {  Tfias_address_type  }
  //Адрес в формате ФИАС
  Tfias_address_type = class(TXmlSerializationObject)
  private
    Faoguid:Taoguid;
    Fhouseguid:Thouseguid;
    Fflat:Tflat;
    procedure Setaoguid( AValue:Taoguid);
    procedure Sethouseguid( AValue:Thouseguid);
    procedure Setflat( AValue:Tflat);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Глобальный уникальный идентификатор адресного объекта
    property aoguid:Taoguid read Faoguid write Setaoguid;
    //Глобальный уникальный идентификатор дома. Обязателен при наличии
    property houseguid:Thouseguid read Fhouseguid write Sethouseguid;
    //Квартира. Обязателен при наличии
    property flat:Tflat read Fflat write Setflat;
  end;

  {  Tshipment_children_products_list_type  }
  Tshipment_children_products_list_type = class(TXmlSerializationObject)
  private
    Fproduct:Tshipment_children_products_list_type_productList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Список КИ/КИТУ в составе транспортной упаковки (отгрузка)
    property product:Tshipment_children_products_list_type_productList read Fproduct;
  end;

  {  Tshipment_children_products_list_type_product  }
  Tshipment_children_products_list_type_product = class(TXmlSerializationObject)
  private
    Fki:Tki_type;
    Fkitu:Tkitu_type;
    Fcost:Tprice_type;
    Fvat_value:Tprice_type;
    Fchildren_products_list:Tshipment_children_products_list_type;
    procedure Setki( AValue:Tki_type);
    procedure Setkitu( AValue:Tkitu_type);
    procedure Setcost( AValue:Tprice_type);
    procedure Setvat_value( AValue:Tprice_type);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //КИ
    property ki:Tki_type read Fki write Setki;
    //КИТУ
    property kitu:Tkitu_type read Fkitu write Setkitu;
    //Цена за единицу
    property cost:Tprice_type read Fcost write Setcost;
    //Сумма НДС
    property vat_value:Tprice_type read Fvat_value write Setvat_value;
    //Список КИ\КИТУ составе транспортной упаковки (отгрузка)
    property children_products_list:Tshipment_children_products_list_type read Fchildren_products_list;
  end;

  {  Tacceptance_children_products_list_type  }
  Tacceptance_children_products_list_type = class(TXmlSerializationObject)
  private
    Fproduct:Tacceptance_children_products_list_type_productList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Список КИ\КИТУ в составе транспортной упаковки (приемка)
    property product:Tacceptance_children_products_list_type_productList read Fproduct;
  end;

  {  Tacceptance_children_products_list_type_product  }
  Tacceptance_children_products_list_type_product = class(TXmlSerializationObject)
  private
    Fki:Tki_type;
    Fkitu:Tkitu_type;
    Faccept_type:Boolean;
    Fchildren_products_list:Tacceptance_children_products_list_type;
    procedure Setki( AValue:Tki_type);
    procedure Setkitu( AValue:Tkitu_type);
    procedure Setaccept_type( AValue:Boolean);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //КИ
    property ki:Tki_type read Fki write Setki;
    //КИТУ
    property kitu:Tkitu_type read Fkitu write Setkitu;
    //Товар принят
    property accept_type:Boolean read Faccept_type write Setaccept_type;
    //Список КИ\КИТУ в составе транспортной упаковки (приемка)
    property children_products_list:Tacceptance_children_products_list_type read Fchildren_products_list;
  end;

  {  Tvvod_children_products_list_type  }
  Tvvod_children_products_list_type = class(TXmlSerializationObject)
  private
    Fproduct:Tvvod_children_products_list_type_productList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Список КИ\КИТУ в составе транспортной упаковки для ввода в оборот
    property product:Tvvod_children_products_list_type_productList read Fproduct;
  end;

  {  Tvvod_children_products_list_type_product  }
  Tvvod_children_products_list_type_product = class(TXmlSerializationObject)
  private
    Fki:Tkit_type;
    Fkitu:Tkitu_type;
    Fproduct_date:Tdate_type;
    Ftnved_code:Ttnved_code_type;
    Fcertificate_type:Tcertificate_type_type;
    Fcertificate_number:Tstring255_type;
    Fcertificate_date:Tdate_type;
    Fvvod_children_products_list:Tvvod_children_products_list_type;
    procedure Setki( AValue:Tkit_type);
    procedure Setkitu( AValue:Tkitu_type);
    procedure Setproduct_date( AValue:Tdate_type);
    procedure Settnved_code( AValue:Ttnved_code_type);
    procedure Setcertificate_type( AValue:Tcertificate_type_type);
    procedure Setcertificate_number( AValue:Tstring255_type);
    procedure Setcertificate_date( AValue:Tdate_type);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //КИ
    property ki:Tkit_type read Fki write Setki;
    //КИТУ
    property kitu:Tkitu_type read Fkitu write Setkitu;
    //Дата производства
    property product_date:Tdate_type read Fproduct_date write Setproduct_date;
    //Код ТН ВЭД ЕАС товара
    property tnved_code:Ttnved_code_type read Ftnved_code write Settnved_code;
    //Вид документа, подтверждающего соответствие
    property certificate_type:Tcertificate_type_type read Fcertificate_type write Setcertificate_type;
    //Номер документа, подтверждающего соответствие
    property certificate_number:Tstring255_type read Fcertificate_number write Setcertificate_number;
    //Дата документа, подтверждающего соответствие
    property certificate_date:Tdate_type read Fcertificate_date write Setcertificate_date;
    //Список КИ\КИТУ в составе транспортной упаковки для ввода в оборот
    property vvod_children_products_list:Tvvod_children_products_list_type read Fvvod_children_products_list;
  end;

  {  Tvvod_ind_children_products_list_type  }
  Tvvod_ind_children_products_list_type = class(TXmlSerializationObject)
  private
    Fproduct:Tvvod_ind_children_products_list_type_productList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Список КИ\КИТУ в составе транспортной упаковки для ввода в оборот (полученных от физических лиц)
    property product:Tvvod_ind_children_products_list_type_productList read Fproduct;
  end;

  {  Tvvod_ind_children_products_list_type_product  }
  Tvvod_ind_children_products_list_type_product = class(TXmlSerializationObject)
  private
    Fki:Tkit_type;
    Fkitu:Tkitu_type;
    Fproduct_receiving_date:Tdate_type;
    Fvvod_ind_children_products_list:Tvvod_ind_children_products_list_type;
    procedure Setki( AValue:Tkit_type);
    procedure Setkitu( AValue:Tkitu_type);
    procedure Setproduct_receiving_date( AValue:Tdate_type);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //КИ
    property ki:Tkit_type read Fki write Setki;
    //КИТУ
    property kitu:Tkitu_type read Fkitu write Setkitu;
    //Дата получения товара
    property product_receiving_date:Tdate_type read Fproduct_receiving_date write Setproduct_receiving_date;
    //Список КИ\КИТУ в составе транспортной упаковки для ввода в оборот (полученных от физических лиц)
    property vvod_ind_children_products_list:Tvvod_ind_children_products_list_type read Fvvod_ind_children_products_list;
  end;

  {  Tvvod_ost_children_products_list_type  }
  Tvvod_ost_children_products_list_type = class(TXmlSerializationObject)
  private
    Fproduct:Tvvod_ost_children_products_list_type_productList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Список КИ\КИТУ в составе транспортной упаковки для ввода в оборот (остатки)
    property product:Tvvod_ost_children_products_list_type_productList read Fproduct;
  end;

  {  Tvvod_ost_children_products_list_type_product  }
  Tvvod_ost_children_products_list_type_product = class(TXmlSerializationObject)
  private
    Fki:Tkit_type;
    Fkitu:Tkitu_type;
    Fvvod_ost_children_products_list:Tvvod_ost_children_products_list_type;
    procedure Setki( AValue:Tkit_type);
    procedure Setkitu( AValue:Tkitu_type);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //КИ
    property ki:Tkit_type read Fki write Setki;
    //КИТУ
    property kitu:Tkitu_type read Fkitu write Setkitu;
    //Список КИ\КИТУ в составе транспортной упаковки для ввода в оборот (остатки)
    property vvod_ost_children_products_list:Tvvod_ost_children_products_list_type read Fvvod_ost_children_products_list;
  end;

  {  Tvvod_crossborder_products_list_type  }
  Tvvod_crossborder_products_list_type = class(TXmlSerializationObject)
  private
    Fproduct:Tvvod_crossborder_products_list_type_productList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Список КИ\КИТУ в составе транспортной упаковки для ввода в оборот (трансграничная торговля)
    property product:Tvvod_crossborder_products_list_type_productList read Fproduct;
  end;

  {  Tvvod_crossborder_products_list_type_product  }
  Tvvod_crossborder_products_list_type_product = class(TXmlSerializationObject)
  private
    Fki:Tkit_type;
    Fkitu:Tkitu_type;
    Ftnved_code:Ttnved_code_type;
    Fcost:Tprice_type;
    Fvat_value:Tprice_type;
    Fcertificate_type:Tcertificate_type_type;
    Fcertificate_number:Tstring255_type;
    Fcertificate_date:Tdate_type;
    Fchildren_products_list:Tvvod_crossborder_products_list_type;
    procedure Setki( AValue:Tkit_type);
    procedure Setkitu( AValue:Tkitu_type);
    procedure Settnved_code( AValue:Ttnved_code_type);
    procedure Setcost( AValue:Tprice_type);
    procedure Setvat_value( AValue:Tprice_type);
    procedure Setcertificate_type( AValue:Tcertificate_type_type);
    procedure Setcertificate_number( AValue:Tstring255_type);
    procedure Setcertificate_date( AValue:Tdate_type);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //КИ
    property ki:Tkit_type read Fki write Setki;
    //КИТУ
    property kitu:Tkitu_type read Fkitu write Setkitu;
    //Код ТН ВЭД ЕАС товара
    property tnved_code:Ttnved_code_type read Ftnved_code write Settnved_code;
    //Цена за единицу
    property cost:Tprice_type read Fcost write Setcost;
    //Сумма НДС
    property vat_value:Tprice_type read Fvat_value write Setvat_value;
    //Вид документа, подтверждающего соответствие
    property certificate_type:Tcertificate_type_type read Fcertificate_type write Setcertificate_type;
    //Номер документа, подтверждающего соответствие
    property certificate_number:Tstring255_type read Fcertificate_number write Setcertificate_number;
    //Дата документа, подтверждающего соответствие
    property certificate_date:Tdate_type read Fcertificate_date write Setcertificate_date;
    //Список КИ/КИТУ в составе транспортной упаковки для ввода в оборот (трансграничная торговля)
    property children_products_list:Tvvod_crossborder_products_list_type read Fchildren_products_list;
  end;

  {  Tvvod_ostatky  }
  //Ввод в оборот. Остатки
  Tvvod_ostatky = class(TXmlSerializationObject)
  private
    Ftrade_participant_inn:Tinn_type;
    Fproducts_list:Tvvod_ostatky_products_list;
    Faction_id:Longint;
    Fversion:Longint;
    procedure Settrade_participant_inn( AValue:Tinn_type);
    procedure Setaction_id( AValue:Longint);
    procedure Setversion( AValue:Longint);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //ИНН участника оборота
    property trade_participant_inn:Tinn_type read Ftrade_participant_inn write Settrade_participant_inn;
    //Параметры товаров
    property products_list:Tvvod_ostatky_products_list read Fproducts_list;
    property action_id:Longint read Faction_id write Setaction_id;
    property version:Longint read Fversion write Setversion;
  end;

  {  Tvvod_ostatky_element  }
  Tvvod_ostatky_element = class(Tvvod_ostatky)
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

  {  Tvvod_ostatky_products_list  }
  Tvvod_ostatky_products_list = class(TXmlSerializationObject)
  private
    Fproduct:Tvvod_ostatky_products_list_productList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Параметры товара
    property product:Tvvod_ostatky_products_list_productList read Fproduct;
  end;

  {  Tvvod_ostatky_products_list_product  }
  Tvvod_ostatky_products_list_product = class(TXmlSerializationObject)
  private
    Fki:Tkit_type;
    Fkitu:Tkitu_type;
    procedure Setki( AValue:Tkit_type);
    procedure Setkitu( AValue:Tkitu_type);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //КИ
    property ki:Tkit_type read Fki write Setki;
    //КИТУ
    property kitu:Tkitu_type read Fkitu write Setkitu;
  end;

implementation

  {  Tfias_address_type  }
procedure Tfias_address_type.Setaoguid(AValue: Taoguid);
begin
  Faoguid:=AValue;
  CheckStrMinSize('aoguid', AValue);
  CheckStrMaxSize('aoguid', AValue);
  ModifiedProperty('aoguid');
end;

procedure Tfias_address_type.Sethouseguid(AValue: Thouseguid);
begin
  Fhouseguid:=AValue;
  CheckStrMinSize('houseguid', AValue);
  CheckStrMaxSize('houseguid', AValue);
  ModifiedProperty('houseguid');
end;

procedure Tfias_address_type.Setflat(AValue: Tflat);
begin
  Fflat:=AValue;
  CheckStrMinSize('flat', AValue);
  CheckStrMaxSize('flat', AValue);
  ModifiedProperty('flat');
end;

procedure Tfias_address_type.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('aoguid', 'aoguid', [xsaSimpleObject], '', 36, 36);
  P:=RegisterProperty('houseguid', 'houseguid', [xsaSimpleObject], '', 36, 36);
  P:=RegisterProperty('flat', 'flat', [xsaSimpleObject], '', 1, 20);
end;

procedure Tfias_address_type.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Tfias_address_type.Destroy;
begin
  inherited Destroy;
end;

constructor Tfias_address_type.Create;
begin
  inherited Create;
end;

  {  Tshipment_children_products_list_type  }
procedure Tshipment_children_products_list_type.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('product', 'product', [], '', -1, -1);
end;

procedure Tshipment_children_products_list_type.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproduct:=Tshipment_children_products_list_type_productList.Create;
end;

destructor Tshipment_children_products_list_type.Destroy;
begin
  Fproduct.Free;
  inherited Destroy;
end;

constructor Tshipment_children_products_list_type.Create;
begin
  inherited Create;
end;

  {  Tshipment_children_products_list_type_product  }
procedure Tshipment_children_products_list_type_product.Setki(AValue: Tki_type);
begin
  Fki:=AValue;
  CheckStrMinSize('ki', AValue);
  CheckStrMaxSize('ki', AValue);
  ModifiedProperty('ki');
end;

procedure Tshipment_children_products_list_type_product.Setkitu(AValue: Tkitu_type);
begin
  Fkitu:=AValue;
  CheckStrMinSize('kitu', AValue);
  CheckStrMaxSize('kitu', AValue);
  ModifiedProperty('kitu');
end;

procedure Tshipment_children_products_list_type_product.Setcost(AValue: Tprice_type);
begin
  Fcost:=AValue;
  CheckMinInclusiveValue('cost', AValue);
  ModifiedProperty('cost');
end;

procedure Tshipment_children_products_list_type_product.Setvat_value(AValue: Tprice_type);
begin
  Fvat_value:=AValue;
  CheckMinInclusiveValue('vat_value', AValue);
  ModifiedProperty('vat_value');
end;

procedure Tshipment_children_products_list_type_product.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('ki', 'ki', [xsaSimpleObject], '', 31, 44);
  P:=RegisterProperty('kitu', 'kitu', [xsaSimpleObject], '', 18, 18);
  P:=RegisterProperty('cost', 'cost', [xsaSimpleObject], '', -1, -1);
    P.TotalDigits := 19;
    P.FractionDigits := 2;
    P.minInclusiveFloat:=0;
  P:=RegisterProperty('vat_value', 'vat_value', [xsaSimpleObject], '', -1, -1);
    P.TotalDigits := 19;
    P.FractionDigits := 2;
    P.minInclusiveFloat:=0;
  P:=RegisterProperty('children_products_list', 'children_products_list', [], '', -1, -1);
end;

procedure Tshipment_children_products_list_type_product.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fchildren_products_list:=Tshipment_children_products_list_type.Create;
end;

destructor Tshipment_children_products_list_type_product.Destroy;
begin
  Fchildren_products_list.Free;
  inherited Destroy;
end;

constructor Tshipment_children_products_list_type_product.Create;
begin
  inherited Create;
end;

  {  Tacceptance_children_products_list_type  }
procedure Tacceptance_children_products_list_type.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('product', 'product', [], '', -1, -1);
end;

procedure Tacceptance_children_products_list_type.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproduct:=Tacceptance_children_products_list_type_productList.Create;
end;

destructor Tacceptance_children_products_list_type.Destroy;
begin
  Fproduct.Free;
  inherited Destroy;
end;

constructor Tacceptance_children_products_list_type.Create;
begin
  inherited Create;
end;

  {  Tacceptance_children_products_list_type_product  }
procedure Tacceptance_children_products_list_type_product.Setki(AValue: Tki_type);
begin
  Fki:=AValue;
  CheckStrMinSize('ki', AValue);
  CheckStrMaxSize('ki', AValue);
  ModifiedProperty('ki');
end;

procedure Tacceptance_children_products_list_type_product.Setkitu(AValue: Tkitu_type);
begin
  Fkitu:=AValue;
  CheckStrMinSize('kitu', AValue);
  CheckStrMaxSize('kitu', AValue);
  ModifiedProperty('kitu');
end;

procedure Tacceptance_children_products_list_type_product.Setaccept_type(AValue: Boolean);
begin
  Faccept_type:=AValue;
  ModifiedProperty('accept_type');
end;

procedure Tacceptance_children_products_list_type_product.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('ki', 'ki', [xsaSimpleObject], '', 31, 44);
  P:=RegisterProperty('kitu', 'kitu', [xsaSimpleObject], '', 18, 18);
  P:=RegisterProperty('accept_type', 'accept_type', [xsaSimpleObject], '', -1, -1);
  P:=RegisterProperty('children_products_list', 'children_products_list', [], '', -1, -1);
end;

procedure Tacceptance_children_products_list_type_product.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fchildren_products_list:=Tacceptance_children_products_list_type.Create;
end;

destructor Tacceptance_children_products_list_type_product.Destroy;
begin
  Fchildren_products_list.Free;
  inherited Destroy;
end;

constructor Tacceptance_children_products_list_type_product.Create;
begin
  inherited Create;
end;

  {  Tvvod_children_products_list_type  }
procedure Tvvod_children_products_list_type.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('product', 'product', [], '', -1, -1);
end;

procedure Tvvod_children_products_list_type.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproduct:=Tvvod_children_products_list_type_productList.Create;
end;

destructor Tvvod_children_products_list_type.Destroy;
begin
  Fproduct.Free;
  inherited Destroy;
end;

constructor Tvvod_children_products_list_type.Create;
begin
  inherited Create;
end;

  {  Tvvod_children_products_list_type_product  }
procedure Tvvod_children_products_list_type_product.Setki(AValue: Tkit_type);
begin
  Fki:=AValue;
  CheckStrMinSize('ki', AValue);
  CheckStrMaxSize('ki', AValue);
  ModifiedProperty('ki');
end;

procedure Tvvod_children_products_list_type_product.Setkitu(AValue: Tkitu_type);
begin
  Fkitu:=AValue;
  CheckStrMinSize('kitu', AValue);
  CheckStrMaxSize('kitu', AValue);
  ModifiedProperty('kitu');
end;

procedure Tvvod_children_products_list_type_product.Setproduct_date(AValue: Tdate_type);
begin
  Fproduct_date:=AValue;
  CheckStrMinSize('product_date', AValue);
  CheckStrMaxSize('product_date', AValue);
  ModifiedProperty('product_date');
end;

procedure Tvvod_children_products_list_type_product.Settnved_code(AValue: Ttnved_code_type);
begin
  Ftnved_code:=AValue;
  CheckMinInclusiveValue('tnved_code', AValue);
  CheckMaxInclusiveValue('tnved_code', AValue);
  ModifiedProperty('tnved_code');
end;

procedure Tvvod_children_products_list_type_product.Setcertificate_type(AValue: Tcertificate_type_type);
begin
  Fcertificate_type:=AValue;
  CheckLockupValue('certificate_type', AValue);
  ModifiedProperty('certificate_type');
end;

procedure Tvvod_children_products_list_type_product.Setcertificate_number(AValue: Tstring255_type);
begin
  Fcertificate_number:=AValue;
  CheckStrMinSize('certificate_number', AValue);
  CheckStrMaxSize('certificate_number', AValue);
  ModifiedProperty('certificate_number');
end;

procedure Tvvod_children_products_list_type_product.Setcertificate_date(AValue: Tdate_type);
begin
  Fcertificate_date:=AValue;
  CheckStrMinSize('certificate_date', AValue);
  CheckStrMaxSize('certificate_date', AValue);
  ModifiedProperty('certificate_date');
end;

procedure Tvvod_children_products_list_type_product.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('ki', 'ki', [xsaSimpleObject], '', 27, 44);
  P:=RegisterProperty('kitu', 'kitu', [xsaSimpleObject], '', 18, 18);
  P:=RegisterProperty('product_date', 'product_date', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('tnved_code', 'tnved_code', [xsaSimpleObject], '', -1, -1);
    P.minInclusiveInt:=6401000000;
    P.maxInclusiveInt:=6405999999;
  P:=RegisterProperty('certificate_type', 'certificate_type', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('CONFORMITY_CERTIFICATE');
    P.ValidList.Add('CONFORMITY_DECLARATION');
  P:=RegisterProperty('certificate_number', 'certificate_number', [xsaSimpleObject], '', 1, 255);
  P:=RegisterProperty('certificate_date', 'certificate_date', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('vvod_children_products_list', 'vvod_children_products_list', [], '', -1, -1);
end;

procedure Tvvod_children_products_list_type_product.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fvvod_children_products_list:=Tvvod_children_products_list_type.Create;
end;

destructor Tvvod_children_products_list_type_product.Destroy;
begin
  Fvvod_children_products_list.Free;
  inherited Destroy;
end;

constructor Tvvod_children_products_list_type_product.Create;
begin
  inherited Create;
end;

  {  Tvvod_ind_children_products_list_type  }
procedure Tvvod_ind_children_products_list_type.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('product', 'product', [], '', -1, -1);
end;

procedure Tvvod_ind_children_products_list_type.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproduct:=Tvvod_ind_children_products_list_type_productList.Create;
end;

destructor Tvvod_ind_children_products_list_type.Destroy;
begin
  Fproduct.Free;
  inherited Destroy;
end;

constructor Tvvod_ind_children_products_list_type.Create;
begin
  inherited Create;
end;

  {  Tvvod_ind_children_products_list_type_product  }
procedure Tvvod_ind_children_products_list_type_product.Setki(AValue: Tkit_type);
begin
  Fki:=AValue;
  CheckStrMinSize('ki', AValue);
  CheckStrMaxSize('ki', AValue);
  ModifiedProperty('ki');
end;

procedure Tvvod_ind_children_products_list_type_product.Setkitu(AValue: Tkitu_type);
begin
  Fkitu:=AValue;
  CheckStrMinSize('kitu', AValue);
  CheckStrMaxSize('kitu', AValue);
  ModifiedProperty('kitu');
end;

procedure Tvvod_ind_children_products_list_type_product.Setproduct_receiving_date(AValue: Tdate_type);
begin
  Fproduct_receiving_date:=AValue;
  CheckStrMinSize('product_receiving_date', AValue);
  CheckStrMaxSize('product_receiving_date', AValue);
  ModifiedProperty('product_receiving_date');
end;

procedure Tvvod_ind_children_products_list_type_product.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('ki', 'ki', [xsaSimpleObject], '', 27, 44);
  P:=RegisterProperty('kitu', 'kitu', [xsaSimpleObject], '', 18, 18);
  P:=RegisterProperty('product_receiving_date', 'product_receiving_date', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('vvod_ind_children_products_list', 'vvod_ind_children_products_list', [], '', -1, -1);
end;

procedure Tvvod_ind_children_products_list_type_product.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fvvod_ind_children_products_list:=Tvvod_ind_children_products_list_type.Create;
end;

destructor Tvvod_ind_children_products_list_type_product.Destroy;
begin
  Fvvod_ind_children_products_list.Free;
  inherited Destroy;
end;

constructor Tvvod_ind_children_products_list_type_product.Create;
begin
  inherited Create;
end;

  {  Tvvod_ost_children_products_list_type  }
procedure Tvvod_ost_children_products_list_type.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('product', 'product', [], '', -1, -1);
end;

procedure Tvvod_ost_children_products_list_type.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproduct:=Tvvod_ost_children_products_list_type_productList.Create;
end;

destructor Tvvod_ost_children_products_list_type.Destroy;
begin
  Fproduct.Free;
  inherited Destroy;
end;

constructor Tvvod_ost_children_products_list_type.Create;
begin
  inherited Create;
end;

  {  Tvvod_ost_children_products_list_type_product  }
procedure Tvvod_ost_children_products_list_type_product.Setki(AValue: Tkit_type);
begin
  Fki:=AValue;
  CheckStrMinSize('ki', AValue);
  CheckStrMaxSize('ki', AValue);
  ModifiedProperty('ki');
end;

procedure Tvvod_ost_children_products_list_type_product.Setkitu(AValue: Tkitu_type);
begin
  Fkitu:=AValue;
  CheckStrMinSize('kitu', AValue);
  CheckStrMaxSize('kitu', AValue);
  ModifiedProperty('kitu');
end;

procedure Tvvod_ost_children_products_list_type_product.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('ki', 'ki', [xsaSimpleObject], '', 27, 44);
  P:=RegisterProperty('kitu', 'kitu', [xsaSimpleObject], '', 18, 18);
  P:=RegisterProperty('vvod_ost_children_products_list', 'vvod_ost_children_products_list', [], '', -1, -1);
end;

procedure Tvvod_ost_children_products_list_type_product.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fvvod_ost_children_products_list:=Tvvod_ost_children_products_list_type.Create;
end;

destructor Tvvod_ost_children_products_list_type_product.Destroy;
begin
  Fvvod_ost_children_products_list.Free;
  inherited Destroy;
end;

constructor Tvvod_ost_children_products_list_type_product.Create;
begin
  inherited Create;
end;

  {  Tvvod_crossborder_products_list_type  }
procedure Tvvod_crossborder_products_list_type.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('product', 'product', [], '', -1, -1);
end;

procedure Tvvod_crossborder_products_list_type.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproduct:=Tvvod_crossborder_products_list_type_productList.Create;
end;

destructor Tvvod_crossborder_products_list_type.Destroy;
begin
  Fproduct.Free;
  inherited Destroy;
end;

constructor Tvvod_crossborder_products_list_type.Create;
begin
  inherited Create;
end;

  {  Tvvod_crossborder_products_list_type_product  }
procedure Tvvod_crossborder_products_list_type_product.Setki(AValue: Tkit_type);
begin
  Fki:=AValue;
  CheckStrMinSize('ki', AValue);
  CheckStrMaxSize('ki', AValue);
  ModifiedProperty('ki');
end;

procedure Tvvod_crossborder_products_list_type_product.Setkitu(AValue: Tkitu_type);
begin
  Fkitu:=AValue;
  CheckStrMinSize('kitu', AValue);
  CheckStrMaxSize('kitu', AValue);
  ModifiedProperty('kitu');
end;

procedure Tvvod_crossborder_products_list_type_product.Settnved_code(AValue: Ttnved_code_type);
begin
  Ftnved_code:=AValue;
  CheckMinInclusiveValue('tnved_code', AValue);
  CheckMaxInclusiveValue('tnved_code', AValue);
  ModifiedProperty('tnved_code');
end;

procedure Tvvod_crossborder_products_list_type_product.Setcost(AValue: Tprice_type);
begin
  Fcost:=AValue;
  CheckMinInclusiveValue('cost', AValue);
  ModifiedProperty('cost');
end;

procedure Tvvod_crossborder_products_list_type_product.Setvat_value(AValue: Tprice_type);
begin
  Fvat_value:=AValue;
  CheckMinInclusiveValue('vat_value', AValue);
  ModifiedProperty('vat_value');
end;

procedure Tvvod_crossborder_products_list_type_product.Setcertificate_type(AValue: Tcertificate_type_type);
begin
  Fcertificate_type:=AValue;
  CheckLockupValue('certificate_type', AValue);
  ModifiedProperty('certificate_type');
end;

procedure Tvvod_crossborder_products_list_type_product.Setcertificate_number(AValue: Tstring255_type);
begin
  Fcertificate_number:=AValue;
  CheckStrMinSize('certificate_number', AValue);
  CheckStrMaxSize('certificate_number', AValue);
  ModifiedProperty('certificate_number');
end;

procedure Tvvod_crossborder_products_list_type_product.Setcertificate_date(AValue: Tdate_type);
begin
  Fcertificate_date:=AValue;
  CheckStrMinSize('certificate_date', AValue);
  CheckStrMaxSize('certificate_date', AValue);
  ModifiedProperty('certificate_date');
end;

procedure Tvvod_crossborder_products_list_type_product.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('ki', 'ki', [xsaSimpleObject], '', 27, 44);
  P:=RegisterProperty('kitu', 'kitu', [xsaSimpleObject], '', 18, 18);
  P:=RegisterProperty('tnved_code', 'tnved_code', [xsaSimpleObject], '', -1, -1);
    P.minInclusiveInt:=6401000000;
    P.maxInclusiveInt:=6405999999;
  P:=RegisterProperty('cost', 'cost', [xsaSimpleObject], '', -1, -1);
    P.TotalDigits := 19;
    P.FractionDigits := 2;
    P.minInclusiveFloat:=0;
  P:=RegisterProperty('vat_value', 'vat_value', [xsaSimpleObject], '', -1, -1);
    P.TotalDigits := 19;
    P.FractionDigits := 2;
    P.minInclusiveFloat:=0;
  P:=RegisterProperty('certificate_type', 'certificate_type', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('CONFORMITY_CERTIFICATE');
    P.ValidList.Add('CONFORMITY_DECLARATION');
  P:=RegisterProperty('certificate_number', 'certificate_number', [xsaSimpleObject], '', 1, 255);
  P:=RegisterProperty('certificate_date', 'certificate_date', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('children_products_list', 'children_products_list', [], '', -1, -1);
end;

procedure Tvvod_crossborder_products_list_type_product.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fchildren_products_list:=Tvvod_crossborder_products_list_type.Create;
end;

destructor Tvvod_crossborder_products_list_type_product.Destroy;
begin
  Fchildren_products_list.Free;
  inherited Destroy;
end;

constructor Tvvod_crossborder_products_list_type_product.Create;
begin
  inherited Create;
end;

  {  Tvvod_ostatky  }
procedure Tvvod_ostatky.Settrade_participant_inn(AValue: Tinn_type);
begin
  Ftrade_participant_inn:=AValue;
  CheckStrMinSize('trade_participant_inn', AValue);
  CheckStrMaxSize('trade_participant_inn', AValue);
  ModifiedProperty('trade_participant_inn');
end;

procedure Tvvod_ostatky.Setaction_id(AValue: Longint);
begin
  Faction_id:=AValue;
  CheckFixedValue('action_id', AValue);
  ModifiedProperty('action_id');
end;

procedure Tvvod_ostatky.Setversion(AValue: Longint);
begin
  Fversion:=AValue;
  CheckFixedValue('version', AValue);
  ModifiedProperty('version');
end;

procedure Tvvod_ostatky.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('trade_participant_inn', 'trade_participant_inn', [xsaSimpleObject], '', 9, 12);
  P:=RegisterProperty('products_list', 'products_list', [], '', -1, -1);
  P:=RegisterProperty('action_id', 'action_id', [xsaRequared], '', -1, -1);
    P.DefaultValue:='5';
  P:=RegisterProperty('version', 'version', [xsaRequared], '', -1, -1);
    P.DefaultValue:='1';
end;

procedure Tvvod_ostatky.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproducts_list:=Tvvod_ostatky_products_list.Create;
end;

destructor Tvvod_ostatky.Destroy;
begin
  Fproducts_list.Free;
  inherited Destroy;
end;

constructor Tvvod_ostatky.Create;
begin
  inherited Create;
  action_id:=5;
  version:=1;
end;

  {  Tvvod_ostatky_element  }
procedure Tvvod_ostatky_element.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
end;

procedure Tvvod_ostatky_element.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Tvvod_ostatky_element.Destroy;
begin
  inherited Destroy;
end;

function Tvvod_ostatky_element.RootNodeName:string;
begin
  Result:='vvod_ostatky';
end;

constructor Tvvod_ostatky_element.Create;
begin
  inherited Create;
end;

  {  Tvvod_ostatky_products_list  }
procedure Tvvod_ostatky_products_list.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('product', 'product', [], '', -1, -1);
end;

procedure Tvvod_ostatky_products_list.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproduct:=Tvvod_ostatky_products_list_productList.Create;
end;

destructor Tvvod_ostatky_products_list.Destroy;
begin
  Fproduct.Free;
  inherited Destroy;
end;

constructor Tvvod_ostatky_products_list.Create;
begin
  inherited Create;
end;

  {  Tvvod_ostatky_products_list_product  }
procedure Tvvod_ostatky_products_list_product.Setki(AValue: Tkit_type);
begin
  Fki:=AValue;
  CheckStrMinSize('ki', AValue);
  CheckStrMaxSize('ki', AValue);
  ModifiedProperty('ki');
end;

procedure Tvvod_ostatky_products_list_product.Setkitu(AValue: Tkitu_type);
begin
  Fkitu:=AValue;
  CheckStrMinSize('kitu', AValue);
  CheckStrMaxSize('kitu', AValue);
  ModifiedProperty('kitu');
end;

procedure Tvvod_ostatky_products_list_product.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('ki', 'ki', [xsaSimpleObject], '', 27, 44);
  P:=RegisterProperty('kitu', 'kitu', [xsaSimpleObject], '', 18, 18);
end;

procedure Tvvod_ostatky_products_list_product.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Tvvod_ostatky_products_list_product.Destroy;
begin
  inherited Destroy;
end;

constructor Tvvod_ostatky_products_list_product.Create;
begin
  inherited Create;
end;

end.