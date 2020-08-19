package systolic

import chisel3._
import chisel3.util._
import scala.math.log10
import chisel3.stage.ChiselStage
//import chisel3.Driver
import chisel3.iotesters.{PeekPokeTester, Driver}
import java.io.PrintWriter
class AddTree(len: Int, width: Int) extends Module{
  def treedep(k: Int): Int = (log10(k-1)/log10(2)).toInt+1
  val io = IO(new Bundle{
    val in = Input(Valid(Vec(len, UInt(width.W))))
    val out = Output(Valid(UInt(width.W)))
  })
  val dep=treedep(len)
  var newlen=len
  val valids = RegInit(VecInit(Seq.fill(dep)(false.B)))
  valids(0) := io.in.valid
  for(i <- 1 until dep){
    valids(i) := valids(i-1)
  }
  val regs=for(i <- 0 until dep) yield{
    newlen=(newlen-1)/2+1
    Reg(Vec(newlen, UInt(width.W)))
  }
  newlen=(len-1)/2+1
  for(j <- 0 until newlen){
    if(j*2+1<len){
      regs(0)(j):=io.in.bits(j*2)+io.in.bits(j*2+1)
    }else{
      regs(0)(j):=io.in.bits(j*2)
    }
  }
  for(i <- 1 until dep){
    for(j <- 0 until (newlen-1)/2+1){
      if(j*2+1<newlen){
        regs(i)(j):=regs(i-1)(j*2)+regs(i-1)(j*2+1)
      }else{
        regs(i)(j):=regs(i-1)(j*2)
      }
    }
    newlen=(newlen-1)/2+1
  }
  io.out.bits:=regs(dep-1)(0)
  io.out.valid := valids(dep-1)
}
class SystolicInput(width: Int) extends Module{
  val io = IO(new Bundle{
    val in = Input(Valid(UInt(width.W)))
    val out = Output(Valid(UInt(width.W)))
    val to_pe = Output(Valid(UInt(width.W)))
  })
  val reg = RegInit(0.U.asTypeOf(Valid(UInt(width.W))))
  reg := io.in
  io.out := reg
  io.to_pe := reg
}
class DirectInput(width: Int) extends Module{
  val io = IO(new Bundle{
    val in = Input(Valid(UInt(width.W)))
    val out = Output(Valid(UInt(width.W)))
    val to_pe = Output(Valid(UInt(width.W)))
  })
  val reg = RegInit(0.U.asTypeOf(Valid(UInt(width.W))))
  reg := io.in
  io.to_pe := reg
  io.out := io.in
}
class DirectOutput(width: Int) extends Module{
  val io = IO(new Bundle{
    val in = Input(Valid(UInt(width.W)))
    val out = Output(Valid(UInt(width.W)))
    val to_pe = Output(Valid(UInt(width.W)))
    val from_pe = Input(Valid(UInt(width.W)))
  })
  io.out := io.from_pe
  io.to_pe := 0.U.asTypeOf(Valid(UInt(width.W)))

}
class SystolicOutput(width: Int) extends Module{
  val io = IO(new Bundle{
    val in = Input(Valid(UInt(width.W)))
    val out = Output(Valid(UInt(width.W)))
    val to_pe = Output(Valid(UInt(width.W)))
    val from_pe = Input(Valid(UInt(width.W)))
  })
  val reg = RegInit(0.U.asTypeOf(Valid(UInt(width.W))))
  reg := io.in
  io.to_pe := reg
  io.out := io.from_pe
}

/*
stationary input:
从右向左传。
stage_cycle: 完成一轮tile计算所需的cycle数。
最左边的PE只接收一个输入。最右边的PE接受所有输入，自己留最后一个，其余的向左传。
stat表示用于PE计算的寄存器，trans表示用来传输的寄存器。当每个PE的一轮计算完成后，令stat=trans，从而无缝开始进行下一轮计算。


*/
class StationaryInput(width: Int) extends Module{
  val io = IO(new Bundle{
    val in = Input(Valid(UInt(width.W)))
    val out = Output(Valid(UInt(width.W)))
    val to_pe = Output(Valid(UInt(width.W)))
    val sig_in2trans = Input(Bool())
    val sig_stat2trans = Input(Bool())
  })
  val trans = RegInit(0.U.asTypeOf(Valid(UInt(width.W))))
  val stat = RegInit(0.U.asTypeOf(Valid(UInt(width.W))))
  val reg_in2trans = RegInit(false.B)
  val reg_stat2trans = RegInit(false.B)
  reg_in2trans := io.sig_in2trans
  reg_stat2trans := io.sig_stat2trans
  io.out := trans
  io.to_pe.bits := stat.bits
  when(reg_in2trans){
    trans := io.in
  }
  when(reg_stat2trans){
    stat := trans
  }
  io.to_pe.valid := stat.valid
}
class StationaryOutput(width: Int) extends Module{
  val io = IO(new Bundle{
    val in = Input(Valid(UInt(width.W)))
    val out = Output(Valid(UInt(width.W)))
    val from_pe = Input(Valid(UInt(width.W)))
    val to_pe = Output(Valid(UInt(width.W)))
    val sig_in2trans = Input(Bool())
    val sig_stat2trans = Input(Bool())
  })
  val trans = RegInit(0.U.asTypeOf(Valid(UInt(width.W))))
  //val stat_C = Module(new RegIO(m*n,width))
  val stat = RegInit(0.U.asTypeOf(Valid(UInt(width.W))))
  val reg_in2trans = RegInit(false.B)
  val reg_stat2trans = RegInit(false.B)
  reg_in2trans := io.sig_in2trans
  reg_stat2trans := io.sig_stat2trans
  //printf("%d %d\n",stat_C, trans_C)
  io.out:=trans
  stat := io.from_pe
  when(reg_stat2trans){
    trans := stat
  }.elsewhen(reg_in2trans){
    trans := io.in
  }
  io.to_pe.valid := true.B
  io.to_pe.bits := Mux(reg_stat2trans, 0.U, stat.bits)
}
object TensorDataflow extends Enumeration{
  type TensorDataflow = Value//这里仅仅是为了将Enumration.Value的类型暴露出来给外界使用而已
  val DirectInputDF, DirectOutputDF, SystolicInputDF, SystolicOutputDF, StationaryInputDF, StationaryOutputDF = Value//在这里定义具体的枚举实例
}
import TensorDataflow._
class PE(m: Int, n: Int, width: Int, dataflowA: TensorDataflow, dataflowB: TensorDataflow, dataflowC: TensorDataflow) extends Module{
  //print(m+" "+n+" "+width+" "+dataflowA+" "+dataflowB+" "+dataflowC)
  val a_stat = (dataflowA == StationaryInputDF || dataflowA == StationaryOutputDF)
  val b_stat = (dataflowB == StationaryInputDF || dataflowB == StationaryOutputDF)
  val c_stat = (dataflowC == StationaryInputDF || dataflowC == StationaryOutputDF)
  val io = IO(new Bundle {
    val in_a = Input(Valid(UInt((m*width).W)))
    val in_b = Input(Valid(UInt((n*width).W)))
    val in_c = Input(Valid(UInt((m*n*width).W)))
    val out_c = Output(Valid(UInt((m*n*width).W)))
    val out_a = Output(Valid(UInt((m*width).W)))
    val out_b = Output(Valid(UInt((n*width).W)))
    val a_sig_in2trans = if(a_stat)Some(Input(Bool()))else None
    val b_sig_in2trans = if(b_stat)Some(Input(Bool()))else None
    val c_sig_in2trans = if(c_stat)Some(Input(Bool()))else None
    val a_sig_stat2trans = if(a_stat)Some(Input(Bool()))else None
    val b_sig_stat2trans = if(b_stat)Some(Input(Bool()))else None
    val c_sig_stat2trans = if(c_stat)Some(Input(Bool()))else None
  })
  val pe = Module(new ComputeCellF(m, n, width)).io
  List((io.in_a, io.out_a, pe.in_a, null,io.a_sig_in2trans, io.a_sig_stat2trans, dataflowA, m), (io.in_b, io.out_b, pe.in_b, null, io.b_sig_in2trans, io.b_sig_stat2trans, dataflowB, n), ( io.in_c, io.out_c, pe.in_c, pe.out_c, io.c_sig_in2trans, io.c_sig_stat2trans, dataflowC, m*n)).map{ case (in, out, pein, peout, sig1, sig2, df, vect)=>
    df match{
      case SystolicInputDF =>{
        val m = Module(new SystolicInput(vect*width)).io
        m.in := in
        out := m.out
        pein := m.to_pe.bits
      }
      case SystolicOutputDF =>{
        val m = Module(new SystolicOutput(vect*width)).io
        m.in := in
        out := m.out
        pein := m.to_pe.bits
        m.from_pe.bits := peout
        m.from_pe.valid := m.to_pe.valid
      }
      case StationaryInputDF =>{
        val m = Module(new StationaryInput(vect*width)).io
        m.in := in
        out := m.out
        pein := m.to_pe.bits
        m.sig_in2trans := sig1.get
        m.sig_stat2trans := sig2.get
      }
      case StationaryOutputDF =>{
        val m = Module(new StationaryOutput(vect*width)).io
        m.in := in
        out := m.out
        pein := m.to_pe.bits
        m.sig_in2trans := sig1.get
        m.sig_stat2trans := sig2.get
        m.from_pe.bits := peout
        m.from_pe.valid := m.to_pe.valid
      }
      case DirectInputDF =>{
        val m = Module(new DirectInput(vect*width)).io
        m.in := in
        out := m.out
        pein := m.to_pe.bits
      }
      case DirectOutputDF =>{
        val m = Module(new DirectOutput(vect*width)).io
        m.in := in
        out := m.out
        pein := m.to_pe.bits
        m.from_pe.bits := peout
        m.from_pe.valid := m.to_pe.valid
      }
    }
  }
}
class OnChipMem(dep: Int, width: Int)extends Module{
  def log2(k: Int): Int = (log10(k-1)/log10(2)).toInt+1
  val addrwidth=log2(dep)
  val io = IO(new Bundle{
    val in_val = Input(Valid(UInt(width.W)))
    val in_addr = Input(UInt(addrwidth.W))
    val out_addr = Input(UInt(addrwidth.W))
    val out_val = Output(Valid(UInt(width.W)))
  })
  val mem = SyncReadMem(dep, UInt(width.W))
  val read_dt = RegInit(0.U(width.W))
  when(io.in_val.valid){
    mem.write( io.in_addr, io.in_val.bits)
  }
  read_dt := mem.read(io.out_addr)
  io.out_val.valid := true.B
  io.out_val.bits := read_dt
}
class PEArray(pe_h: Int, pe_w: Int, width: Int, vecA: Array[Int], vecB: Array[Int], vecC: Array[Int], veca: Int, vecb: Int) extends Module{
  def calc_len(x: Int, y: Int): Int = {
    if(x==0)
      pe_w
    else if(y==0)
      pe_h
    else
      pe_h + pe_w - 1
  }
  val a_io_len = calc_len(vecA(0), vecA(1))
  val b_io_len = calc_len(vecB(0), vecB(1))
  val c_io_len = calc_len(vecC(0), vecC(1))
  val io = IO(new Bundle{
    //val inst = DeqIO(new RoCCInstruction()) //指令输入
    val in_a = Input(Vec(a_io_len, Valid(UInt((veca*width).W)))) //数据输入
    val in_b = Input(Vec(b_io_len, Valid(UInt((vecb*width).W))))
    val out_c = Output(Vec(c_io_len, Valid(UInt((veca*vecb*width).W))))
    val work = Input(Bool())
    val stage_cycle = Input(UInt(9.W))
  })
  //val reg_work = RegInit(VecInit(Seq.fill(a_io_len + b_io_len)(false.B)))
  // reg_work(0) := io.work
  // for(i <- 1 until (a_io_len + b_io_len)){
  //   reg_work(i) := reg_work(i-1)
  // }
  val dfs = List((vecA, 0), (vecB, 0), (vecC, 1)).map{case (y, z)=>
    val xy_diff = y(0)!=0||y(1)!=0
    val t_diff = y(2)!=0
    var ret = StationaryInputDF
    if(!xy_diff && t_diff){
      if(z==0){
        ret = StationaryInputDF
      }else{
        ret = StationaryOutputDF
      }
    }else if(xy_diff && t_diff){
      if(z==0){
        ret = SystolicInputDF
      }else{
        ret = SystolicOutputDF
      }
    }else{
      if(z==0){
        ret = DirectInputDF
      }else{
        ret = DirectOutputDF
      }
    }
    ret
  }
  
  val dataflowA = dfs(0)
  val dataflowB = dfs(1)
  val dataflowC = dfs(2)
  //print("DFS:"+dataflowA+" "+dataflowB+" "+dataflowC)
  val a_stat = (dataflowA == StationaryInputDF || dataflowA == StationaryOutputDF)
  val b_stat = (dataflowB == StationaryInputDF || dataflowB == StationaryOutputDF)
  val c_stat = (dataflowC == StationaryInputDF || dataflowC == StationaryOutputDF)
  
  val pes = for(i <- 0 until pe_h) yield{
    for(j <- 0 until pe_w) yield{
      Module(new PE(veca, vecb, width,dataflowA, dataflowB, dataflowC)).io
    }
  }
  val cur_cycle = for(i <- 0 until pe_h) yield{
    for(j <- 0 until pe_w) yield{
      RegInit(0.U(9.W))
    }
  }
  val reg_work = for(i <- 0 until pe_h) yield{
    for(j <- 0 until pe_w) yield{
      RegInit(false.B)
    }
  }
  cur_cycle(0)(0):= Mux(cur_cycle(0)(0)+io.work===io.stage_cycle, 0.U, cur_cycle(0)(0)+io.work)
  reg_work(0)(0) := io.work
  for(i <- 0 until pe_h){
    if(i!=0){
      cur_cycle(i)(0) := cur_cycle(i-1)(0)
      reg_work(i)(0) := reg_work(i-1)(0)
    }
      
    for(j <- 1 until pe_w){
      cur_cycle(i)(j) := cur_cycle(i)(j-1)
      reg_work(i)(j) := reg_work(i)(j-1)
    }
  }
  // stat input
  if(vecA(0)==0&&vecA(1)==0){
    for(i <- 0 until pe_h){
      for(j <- 1 until pe_w){
        pes(i)(j).out_a <> pes(i)(j-1).in_a
      }
      pes(i)(pe_w-1).in_a := io.in_a(i)
    }
  }
  if(vecA(0)==0&&vecA(1)==1){
    for(j <- 0 until pe_w){
      for(i <- 1 until pe_h){
        pes(i)(j).in_a <> pes(i-1)(j).out_a
      }
      pes(0)(j).in_a := io.in_a(j)
    }
  }
  if(vecA(0)==1&&vecA(1)==0){
    for(i <- 0 until pe_h){
      for(j <- 1 until pe_w){
        pes(i)(j).in_a <> pes(i)(j-1).out_a
      }
      pes(i)(0).in_a := io.in_a(i)
    }
  }
  if(vecA(0)==1&&vecA(1)==1){
    var in_id = 0
    for(i <- 0 until pe_h){
      for(j <- 0 until pe_w){
        if(i-1>=0&&j-1>=0){
          pes(i)(j).in_a <> pes(i-1)(j-1).out_a
        }else{
          pes(i)(j).in_a := io.in_a(in_id)
          in_id = in_id + 1
        }
        
      }
    }
  }
  if(vecB(0)==0&&vecB(1)==0){
    for(i <- 0 until pe_h){
      for(j <- 1 until pe_w){
        pes(i)(j).out_b <> pes(i)(j-1).in_b
      }
      pes(i)(pe_w-1).in_b := io.in_b(i)
    }
  }
  if(vecB(0)==0&&vecB(1)==1){
    for(j <- 0 until pe_w){
      for(i <- 1 until pe_h){
        pes(i)(j).in_b <> pes(i-1)(j).out_b
      }
      pes(0)(j).in_b := io.in_b(j)
    }
  }
  if(vecB(0)==1&&vecB(1)==0){
    for(i <- 0 until pe_h){
      for(j <- 1 until pe_w){
        pes(i)(j).in_b <> pes(i)(j-1).out_b
      }
      pes(i)(0).in_b := io.in_b(i)
    }
  }
  if(vecB(0)==1&&vecB(1)==1){
    var in_id = 0
    for(i <- 0 until pe_h){
      for(j <- 0 until pe_w){
        if(i-1>=0&&j-1>=0){
          pes(i)(j).in_b <> pes(i-1)(j-1).out_b
        }else{
          pes(i)(j).in_b := io.in_b(in_id)
          in_id = in_id + 1
        }
      }
    }
  }
  // systolic, vertical
  if(vecC(0)==0&&vecC(2)==1){
    for(j <- 0 until pe_w){
      for(i <- 1 until pe_h){
        pes(i)(j).in_c <> pes(i-1)(j).out_c
      }
      pes(0)(j).in_c.bits := 0.U
      pes(0)(j).in_c.valid := true.B
      io.out_c(j) := pes(pe_h-1)(j).out_c
    }
  }
  //systolic, horizontal
  if(vecC(0)==1&&vecC(1)==0&&vecC(2)==1){
    for(i <- 0 until pe_h){
      for(j <- 1 until pe_w){
        pes(i)(j).in_c <> pes(i)(j-1).out_c
      }
      pes(i)(0).in_c.bits := 0.U
      pes(i)(0).in_c.valid := true.B
      io.out_c(i) := pes(i)(pe_w-1).out_c
    }
  }
  // 对角线，systolic
  if(vecC(0)==1&&vecC(1)==1&&vecC(2)==1){
    var out_id = 0
    for(i <- 0 until pe_h){
      for(j <- 0 until pe_w){
        if(i-1>0&&j-1>0){
          pes(i)(j).in_c <> pes(i-1)(j-1).out_c
        }else{
          pes(i)(j).in_c.bits := 0.U
          pes(i)(j).in_c.valid := false.B
        }
        if(i+1>=pe_h || j+1 >= pe_w){
          io.out_c(out_id) := pes(i)(j).out_c
        }
      }
    }
  }
  // reduction tree
  if(vecC(0)==0&&vecC(1)==1&&vecC(2)==0){
    for(i <- 0 until pe_w){
      val tree = Module(new AddTree(pe_h, width))
      tree.io.in.valid := pes(0)(i).out_c.valid
      for(j <- 0 until pe_h){
        pes(j)(i).in_c.bits := 0.U
        pes(j)(i).in_c.valid := false.B
        tree.io.in.bits(j) := pes(j)(i).out_c.bits
      }
      io.out_c(i) := tree.io.out
    }
  }
  if(vecC(0)==1&&vecC(1)==0&&vecC(2)==0){
    for(i <- 0 until pe_h){
      val tree = Module(new AddTree(pe_w, width))
      tree.io.in.valid := pes(0)(i).out_c.valid
      for(j <- 0 until pe_w){
        pes(i)(j).in_c.bits := 0.U
        pes(i)(j).in_c.valid := false.B
        tree.io.in.bits(j) := pes(i)(j).out_c.bits
      }
      io.out_c(i) := tree.io.out
    }
  }
  for(i <- 0 until pe_h){
    for(j <- 0 until pe_w){
      if(a_stat){
        pes(i)(j).a_sig_in2trans.get := (cur_cycle(i)(j)>0.U&&cur_cycle(i)(j)<=i.asUInt&&reg_work(i)(j))
        pes(i)(j).a_sig_stat2trans.get := (cur_cycle(i)(j)===0.U&&reg_work(i)(j))
      }
      if(b_stat){
        pes(i)(j).b_sig_in2trans.get := (cur_cycle(i)(j)>=(io.stage_cycle - (i+1).asUInt)&&reg_work(i)(j))
        pes(i)(j).b_sig_stat2trans.get := (cur_cycle(i)(j)===0.U&&reg_work(i)(j))
      }
      if(c_stat){
        pes(i)(j).c_sig_in2trans.get := (cur_cycle(i)(j)>0.U&&cur_cycle(i)(j)<=i.asUInt&&reg_work(i)(j))
        pes(i)(j).c_sig_stat2trans.get := (cur_cycle(i)(j)===0.U&&reg_work(i)(j))
      }
    }
  }
  // printf("%d\n",cur_cycle(0)(0))
  // for(i <- 0 until pe_h){
  //   for(j <- 0 until pe_w){
  //     //printf("(%d %d %d %d)", pes(i)(j).out_c.bits, cur_cycle(i)(j), pes(i)(j).b_sig_in2trans.get, pes(i)(j).b_sig_stat2trans.get)
  //     printf("(%d %d %d %d)", pes(i)(j).in_b.bits, pes(i)(j).out_b.bits, pes(i)(j).b_sig_in2trans.get, pes(i)(j).b_sig_stat2trans.get)
  //   }
  //   printf("\n")
  // }
  
  // for(i <- 0 until 4){
  //   printf("%d ",io.out_c(i).bits)
  // }
  // printf("\n")
}
class PEArrayMemF_WS(pe_h: Int, pe_w: Int, width: Int, vecA: Array[Int], vecB: Array[Int], vecC: Array[Int], veca: Int, vecb: Int) extends Module{
  def calc_len(x: Int, y: Int): Int = {
    if(x==0)
      pe_w
    else if(y==0)
      pe_h
    else
      pe_h + pe_w - 1
  }
  val a_io_len = calc_len(vecA(0), vecA(1))
  val b_io_len = calc_len(vecB(0), vecB(1))
  val c_io_len = calc_len(vecC(0), vecC(1))
  val io = IO(new Bundle{
    //val inst = DeqIO(new RoCCInstruction()) //指令输入
    val in_a = Input(Vec(a_io_len, Valid(UInt((veca*width).W)))) //数据输入
    val in_b = Input(Vec(b_io_len, Valid(UInt((vecb*width).W))))
    val out_c = Output(Vec(c_io_len, Valid(UInt((veca*vecb*width).W))))
    val work = Input(Bool())
    val stage_cycle = Input(UInt(9.W))
  })

  val wr_addr_a = RegInit(VecInit(Seq.fill(a_io_len)(0.U(10.W))))
  val wr_addr_b = RegInit(VecInit(Seq.fill(b_io_len)(0.U(10.W))))
  val wr_addr_c = RegInit(VecInit(Seq.fill(c_io_len)(0.U(10.W))))
  val rd_addr_a = RegInit(VecInit(Seq.fill(a_io_len)(256.U(10.W))))
  val rd_addr_b = RegInit(VecInit(Seq.fill(b_io_len)(256.U(10.W))))
  val rd_addr_c = RegInit(VecInit(Seq.fill(c_io_len)(256.U(10.W))))
  val pearray = Module(new PEArray(pe_h, pe_w, width, vecA, vecB, vecC, veca, vecb)).io
  val mem_a = for(i <- 0 until a_io_len)yield{
    Module(new OnChipMem(1024, veca*width)).io
  }
  val mem_b = for(i <- 0 until b_io_len)yield{
    Module(new OnChipMem(1024, vecb*width)).io
  }
  val mem_c = for(i <- 0 until c_io_len)yield{
    Module(new OnChipMem(1024, veca*vecb*width)).io
  }
  for(i <- 0 until a_io_len){
    mem_a(i).in_val := io.in_a(i)
    mem_a(i).in_addr := wr_addr_a(i)
    mem_a(i).out_addr := rd_addr_a(i)
    pearray.in_a(i) := mem_a(i).out_val
  }
  
  for(i <- 0 until b_io_len){
    mem_b(i).in_val := io.in_b(i)
    mem_b(i).in_addr := wr_addr_b(i)
    mem_b(i).out_addr := rd_addr_b(i)
    pearray.in_b(i) := mem_b(i).out_val
  }
  for(i <- 0 until c_io_len){
    mem_c(i).in_val := pearray.out_c(i)
    mem_c(i).in_addr := wr_addr_c(i)
    mem_c(i).out_addr := rd_addr_c(i)
    io.out_c(i) := mem_c(i).out_val
  }
  

  pearray.work := io.work
  pearray.stage_cycle := io.stage_cycle 
  for(i <- 0 until a_io_len){
    if(vecA(2)==0){
      rd_addr_a(i) := Mux(rd_addr_a(i)+io.work === 512.U, 0.U, rd_addr_a(i)+io.work)
    }
    else{
      if(i!=0)
        rd_addr_a(i) := rd_addr_a(i-1)
      else
        rd_addr_a(i) := Mux(rd_addr_a(i)+io.work === 512.U, 0.U, rd_addr_a(i)+io.work)
    }
  }
  for(i <- 0 until b_io_len){
    if(vecB(2)==0){
      rd_addr_b(i) := Mux(rd_addr_b(i)+io.work === 512.U, 0.U, rd_addr_b(i)+io.work)
    }
    else{
      if(i!=0)
        rd_addr_b(i) := rd_addr_b(i-1)
      else
        rd_addr_b(i) := Mux(rd_addr_b(i)+io.work === 512.U, 0.U, rd_addr_b(i)+io.work)
    }
  }
  for(i <- 0 until c_io_len){
    if(vecC(2)==0){
      rd_addr_c(i) := Mux(rd_addr_c(i)+io.work === 512.U, 0.U, rd_addr_c(i)+io.work)
    }
    else{
      if(i!=0)
        rd_addr_c(i) := rd_addr_c(i-1)
      else
        rd_addr_c(i) := Mux(rd_addr_c(i)+io.work === 512.U, 0.U, rd_addr_c(i)+io.work)
    }
  }

  for(i <- 0 until a_io_len){
    if(vecA(2)==0){
      wr_addr_a(i) := Mux(wr_addr_a(i)+io.work === 512.U, 0.U, wr_addr_a(i)+io.work)
    }
    else{
      if(i!=0)
        wr_addr_a(i) := wr_addr_a(i-1)
      else
        wr_addr_a(i) := Mux(wr_addr_a(i)+io.work === 512.U, 0.U, wr_addr_a(i)+io.work)
    }
  }
  for(i <- 0 until b_io_len){
    if(vecB(2)==0){
      wr_addr_b(i) := Mux(wr_addr_b(i)+io.work === 512.U, 0.U, wr_addr_b(i)+io.work)
    }
    else{
      if(i!=0)
        wr_addr_b(i) := wr_addr_b(i-1)
      else
        wr_addr_b(i) := Mux(wr_addr_b(i)+io.work === 512.U, 0.U, wr_addr_b(i)+io.work)
    }
  }
  for(i <- 0 until c_io_len){
    if(vecC(2)==0){
      wr_addr_c(i) := Mux(wr_addr_c(i)+io.work === 512.U, 0.U, wr_addr_c(i)+io.work)
    }
    else{
      if(i!=0)
        wr_addr_c(i) := wr_addr_c(i-1)
      else
        wr_addr_c(i) := Mux(wr_addr_c(i)+io.work === 512.U, 0.U, wr_addr_c(i)+io.work)
    }
  }
}
class Test_Res1(c: PEArray) extends PeekPokeTester(c){
  poke(c.io.work, 1)
  poke(c.io.stage_cycle,12)
  for(j <- 0 until 2){
    for(i <- 0 until 64){
      for(k <- 0 until 4){
        poke(c.io.in_a(k).bits, 1)
        poke(c.io.in_b(k).bits, i)
        poke(c.io.in_a(k).valid, 1)
        poke(c.io.in_b(k).valid, 1)
      }
      step(1)
    }
  }
  // output: C(1, 4), C(2, 4), C(3, 4), C(4, 4), C(1, 3), C(2, 3), C(3, 3), C(4, 3)...
}
object TestMat{
  import scala.collection.mutable.Set
  
  def test(mat: Array[Array[Int]]):Boolean={
    var ret = true
    if(mat(2)(0)==1&&mat(2)(1)==1&&mat(2)(2)==0){
      ret = false
    }
    val s = Set[(Int,Int,Int)]()
    for (i <- 0 until 3){
      for (j <- 0 until 3){
        for (k <- 0 until 3){
          val x = i * mat(0)(0)+j*mat(0)(1)+k*mat(0)(2)
          val y = i * mat(1)(0)+j*mat(1)(1)+k*mat(1)(2)
          val z = i * mat(2)(0)+j*mat(2)(1)+k*mat(2)(2)
          if(s.contains((x,y,z))){
            ret=false
          }
          s.add((x,y,z))
        }
      }
    }
    ret
  }
}
object Test extends App {
  //chisel3.Driver.execute(args, () => new PE(1, 1, 16, SystolicInputDF, SystolicInputDF, StationaryOutputDF))
  //chisel3.Driver.execute(args, () => new PEArrayWS(16, 16, 16, Array(1, 0, 1), Array(0, 0, 1), Array(0, 1, 1)) )
  var dfid = 0
  var mat = Array(Array(0,1,0),Array(1,0,0),Array(0,0,1))
  
  chisel3.Driver.execute(args, () => new PEArrayMemF_WS(16,10,32,mat(0),mat(1),mat(2),8, 1))
  //Driver(() => new PEArray(4,4,16,mat(0),mat(1),mat(2),1,1))(c => new Test_Res1(c))
  // for(m <- 0 until 512){
  //   var g = m
  //   for(i <- 0 until 3){
  //     for(j <- 0 until 3){
  //       mat(i)(j)=g%2
  //       g =g/2
  //     }
  //   }
  //   if(TestMat.test(mat)){
  //     println(m,dfid,"A:",mat(0).mkString(","),"B:",mat(1).mkString(","),"C:",mat(2).mkString(","))
  //     val str = chisel3.Driver.emitVerilog(new PEArray(16,16,16,mat(0),mat(1),mat(2),1,1))
  //     new PrintWriter("PEArray_"+dfid+".v") { write(str); close }
  //     dfid=dfid+1
  //   }
  // }
  //val str = chisel3.Driver.emitVerilog(new PEArray2() )
  //print(str)
  // chisel3.Driver.execute(args, () => new PEArray3() )
  // chisel3.Driver.execute(args, () => new PEArray4() )
  // chisel3.Driver.execute(args, () => new PEArray5() )
  // chisel3.Driver.execute(args, () => new PEArray6() )
  // chisel3.Driver.execute(args, () => new PEArray7() )
  //chisel3.Driver.execute(args, () => new PEArrayGen() )
}
