package com.satish.shop.domain

import java.util.UUID

object brand:

  opaque type BrandId  = UUID
  opaque type BrandName = String

  case class Brand(id : BrandId, name: BrandName)
