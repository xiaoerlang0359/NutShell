/**************************************************************************************
* Copyright (c) 2020 Institute of Computing Technology, CAS
* Copyright (c) 2020 University of Chinese Academy of Sciences
* 
* NutShell is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2. 
* You may obtain a copy of Mulan PSL v2 at:
*             http://license.coscl.org.cn/MulanPSL2 
* 
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER 
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR 
* FIT FOR A PARTICULAR PURPOSE.  
*
* See the Mulan PSL v2 for more details.  
***************************************************************************************/

package device

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile

import nutcore.HasNutCoreParameter
import bus.axi4._
import utils._

class SramIO extends Bundle with HasNutCoreParameter {
  val rIdx  = Output(UInt(DataBits.W))
  val rdata = Input(UInt(DataBits.W))
  val wIdx  = Output(UInt(DataBits.W))
  val wdata = Output(UInt(DataBits.W))
  val wmask = Output(UInt(DataBits.W))
  val wen   = Output(Bool())
  val ren   = Output(Bool())
}

class AXI4RAM[T <: AXI4Lite](_type: T = new AXI4, memByte: Int, useBlackBox: Boolean = false)
  extends AXI4SlaveModule(_type, if (useBlackBox) new SramIO else null) with HasNutCoreParameter {

  val offsetBits = log2Up(memByte)
  val offsetMask = (1 << offsetBits) - 1
  def index(addr: UInt) = (addr & offsetMask.U) >> log2Ceil(DataBytes)
  def inRange(idx: UInt) = idx < (memByte / 8).U

  val wIdx = index(waddr) + writeBeatCnt
  val rIdx = index(raddr) + readBeatCnt
  val wen = in.w.fire() && inRange(wIdx)

  val rdata = if (useBlackBox) {
    io.extra.get.rIdx := rIdx
    io.extra.get.wIdx := wIdx
    io.extra.get.wdata := in.w.bits.data
    io.extra.get.wmask := fullMask
    io.extra.get.wen := wen
    io.extra.get.ren := ren
    io.extra.get.rdata
  } else {
    val mem = Mem(memByte / DataBytes, Vec(DataBytes, UInt(8.W)))

    val wdata = VecInit.tabulate(DataBytes) { i => in.w.bits.data(8*(i+1)-1, 8*i) }
    when (wen) { mem.write(wIdx, wdata, in.w.bits.strb.asBools) }

    Cat(mem.read(rIdx).reverse)
  }

  in.r.bits.data := RegEnable(rdata, ren)
}
