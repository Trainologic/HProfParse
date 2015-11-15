package com.trainologic.com

import com.trainologic.com.phase1.Tag
import com.trainologic.com.phase1.UTF8
package object phase2 {
  def strings(tags: List[Tag]) = tags.collect {
      case UTF8(_, id, str) => (id, str)
    }.toMap

}