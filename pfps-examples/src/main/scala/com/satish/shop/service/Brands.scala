package com.satish.shop.service
import com.satish.shop.domain.brand.*

import java.util.UUID

trait Brands[F[_]]:
  def findAll : F[List[Brand]]
  def add(name: BrandName) : F[BrandId]


