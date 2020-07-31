package systolic

import chisel3._
import chisel3.util._
import scala.math.log10
//import chisel3.Driver
import chisel3.iotesters.{PeekPokeTester, Driver}
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
  io.to_pe := io.in
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
  val input_cycle = RegInit(0.U(10.W))
  val in_valid = RegInit(false.B)
  io.out := trans
  io.to_pe.bits := stat.bits
  when(io.sig_in2trans){
    trans := io.in
  }
  when(io.sig_stat2trans){
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
  //printf("%d %d\n",stat_C, trans_C)
  io.out:=trans
  stat := io.from_pe
  when(io.sig_stat2trans){
    trans := stat
  }
  when(io.sig_in2trans){
    trans := io.in
  }
  io.to_pe.valid := stat.valid
  io.to_pe.bits := Mux(io.sig_stat2trans, 0.U, stat.bits)
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
    val valid = Input(Bool())
  })
  val pe = Module(new ComputeCell(m, n, width)).io
  pe.valid := io.valid
  List((io.in_a, io.out_a, pe.in_a, null,io.a_sig_in2trans, io.a_sig_stat2trans, dataflowA), (io.in_b, io.out_b, pe.in_b, null, io.b_sig_in2trans, io.b_sig_stat2trans, dataflowB), ( io.in_c, io.out_c, pe.in_c, pe.out_c, io.c_sig_in2trans, io.c_sig_stat2trans, dataflowC)).map{ case (in, out, pein, peout, sig1, sig2, df)=>
    df match{
      case SystolicInputDF =>{
        val m = Module(new SystolicInput(width)).io
        m.in := in
        out := m.out
        pein := m.to_pe
      }
      case SystolicOutputDF =>{
        val m = Module(new SystolicOutput(width)).io
        m.in := in
        out := m.out
        pein := m.to_pe
        m.from_pe := peout
      }
      case StationaryInputDF =>{
        val m = Module(new StationaryInput(width)).io
        m.in := in
        out := m.out
        pein := m.to_pe
        m.sig_in2trans := sig1.get
        m.sig_stat2trans := sig2.get
      }
      case StationaryOutputDF =>{
        val m = Module(new StationaryOutput(width)).io
        m.in := in
        out := m.out
        pein := m.to_pe
        m.sig_in2trans := sig1.get
        m.sig_stat2trans := sig2.get
        m.from_pe:=peout
      }
      case DirectInputDF =>{
        val m = Module(new DirectInput(width)).io
        m.in := in
        out := m.out
        pein := m.to_pe
      }
      case DirectOutputDF =>{
        val m = Module(new DirectOutput(width)).io
        m.in := in
        out := m.out
        pein := m.to_pe
        m.from_pe := peout
      }
    }
  }
}
// class DummyPE(m: Int, n: Int, width: Int, dataflowA: TensorDataflow, dataflowB: TensorDataflow, dataflowC: TensorDataflow) extends Module{
//   val a_stat = false
//   val b_stat = false
//   val c_stat = true
//   val io = IO(new Bundle {
//     val in_a = Input(Valid(UInt((m*width).W)))
//     val in_b = Input(Valid(UInt((n*width).W)))
//     val in_c = Input(Valid(UInt((m*n*width).W)))
//     val out_c = Output(Valid(UInt((m*n*width).W)))
//     val out_a = Output(Valid(UInt((m*width).W)))
//     val out_b = Output(Valid(UInt((n*width).W)))
//     val a_sig_in2trans = if(a_stat)Some(Input(Bool()))else None
//     val b_sig_in2trans = if(b_stat)Some(Input(Bool()))else None
//     val c_sig_in2trans = if(c_stat)Some(Input(Bool()))else None
//     val a_sig_stat2trans = if(a_stat)Some(Input(Bool()))else None
//     val b_sig_stat2trans = if(b_stat)Some(Input(Bool()))else None
//     val c_sig_stat2trans = if(c_stat)Some(Input(Bool()))else None
//     val valid = Input(Bool())
//   })
//   io.out_c := io.in_c
//   io.out_a := io.in_a
//   io.out_b := io.in_b
//   when(io.valid){
//     io.out_c.bits := io.in_c.bits + io.in_a.bits * io.in_b.bits
//   }
// }
class DummyPE(m: Int, n: Int, width: Int, dataflowA: TensorDataflow, dataflowB: TensorDataflow, dataflowC: TensorDataflow) extends Module{
  val a_stat = false
  val b_stat = false
  val c_stat = true
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
    val valid = Input(Bool())
  })
  val pe = Module(new ComputeCell(m, n, width)).io
  pe.valid := io.valid
  val v = Module(new SystolicInput(width)).io
  v.in := io.in_a
  io.out_a := v.out
  pe.in_a := v.to_pe
  val y = Module(new SystolicInput(width)).io
  y.in := io.in_b
  io.out_b := y.out
  pe.in_b := y.to_pe
  val q = Module(new StationaryOutput(width)).io
  q.in := io.in_c
  io.out_c := q.out
  pe.in_c := q.to_pe
  q.sig_in2trans := io.c_sig_in2trans.get
  q.sig_stat2trans := io.c_sig_stat2trans.get
  q.from_pe:=pe.out_c
}
// class BlackPE(m: Int, n: Int, width: Int, dataflowA: TensorDataflow, dataflowB: TensorDataflow, dataflowC: TensorDataflow) extends BlackBox{
//   //print(m+" "+n+" "+width+" "+dataflowA+" "+dataflowB+" "+dataflowC)
//   val a_stat = (dataflowA == StationaryInputDF || dataflowA == StationaryOutputDF)
//   val b_stat = (dataflowB == StationaryInputDF || dataflowB == StationaryOutputDF)
//   val c_stat = (dataflowC == StationaryInputDF || dataflowC == StationaryOutputDF)
//   val io = IO(new Bundle {
//     val in_a = Input(Valid(UInt((m*width).W)))
//     val in_b = Input(Valid(UInt((n*width).W)))
//     val in_c = Input(Valid(UInt((m*n*width).W)))
//     val out_c = Output(Valid(UInt((m*n*width).W)))
//     val out_a = Output(Valid(UInt((m*width).W)))
//     val out_b = Output(Valid(UInt((n*width).W)))
//     val a_sig_in2trans = if(a_stat)Some(Input(Bool()))else None
//     val b_sig_in2trans = if(b_stat)Some(Input(Bool()))else None
//     val c_sig_in2trans = if(c_stat)Some(Input(Bool()))else None
//     val a_sig_stat2trans = if(a_stat)Some(Input(Bool()))else None
//     val b_sig_stat2trans = if(b_stat)Some(Input(Bool()))else None
//     val c_sig_stat2trans = if(c_stat)Some(Input(Bool()))else None
//     val valid = Input(Bool())
//   })
  
// }
class PEArrayGen() extends Module{
  val io = IO(new Bundle{
    //val inst = DeqIO(new RoCCInstruction()) //指令输入
    val in_a = Input(Valid(Vec(16, UInt((16).W)))) //数据输入
    val in_b = Input(Valid(Vec(16, UInt((16).W)))) 
    val out_c = Output(Valid(Vec(16, UInt((16).W))))
    val work = Input(Bool())
  })
  val pes = for(i <- 0 until 16) yield{
    for(j <- 0 until 16) yield{
      Module(new DummyPE(1, 1, 16,SystolicInputDF, SystolicInputDF, StationaryOutputDF)).io
    }
  }
  val reg_work = RegInit(VecInit(Seq.fill(16)(false.B)))
  reg_work(0) := io.work
  for(i <- 1 until 16){
    reg_work(i) := reg_work(i-1)
  }
  // for(i <- 0 until 16){
  //   for(j <- 0 until 16){
  //     pes(i)(j).in_a.bits := io.in_a.bits(i)
  //     pes(i)(j).in_a.valid := reg_work(i)
  //   }
  // }
  for(j <- 0 until 16){
    pes(0)(j).in_a.bits := io.in_a.bits(j)
    pes(0)(j).in_a.valid := reg_work(j)
    for(i <- 1 until 16){
      pes(i)(j).in_a <> pes(i-1)(j).out_a
    }
    
  }
  for(i <- 0 until 16){
    pes(i)(0).in_b.bits := io.in_b.bits(i)
    pes(i)(0).in_b.valid := reg_work(i)
    for(j <- 1 until 16){
      pes(i)(j).in_b <> pes(i)(j-1).out_b
    }
    
  }
  for(j <- 0 until 16){
    for(i <- 1 until 16){
      pes(i)(j).in_c <> pes(i-1)(j).out_c
    }
    pes(0)(j).in_c.bits := 0.U
    pes(0)(j).in_c.valid := true.B
    io.out_c.bits(j) := pes(15)(j).out_c.bits
  }
  for(i <- 0 until 16){
    for(j <- 0 until 16){
      pes(i)(j).valid := true.B
      pes(i)(j).c_sig_in2trans.get := true.B
      pes(i)(j).c_sig_stat2trans.get := true.B
    }
  }
  io.out_c.valid := true.B
}
class PEArray(pe_h: Int, pe_w: Int, width: Int, vecA: Array[Int], vecB: Array[Int], vecC: Array[Int]) extends Module{
  def calc_len(x: Int, y: Int): Int = {
    if(y==0)
      pe_h
    else if(x==0&&y!=0)
      pe_w
    else
      pe_h + pe_w - 1
  }
  val a_io_len = calc_len(vecA(0), vecA(1))
  val b_io_len = calc_len(vecB(0), vecB(1))
  val c_io_len = calc_len(vecC(0), vecC(1))
  val io = IO(new Bundle{
    //val inst = DeqIO(new RoCCInstruction()) //指令输入
    val in_a = DeqIO(Vec(a_io_len, UInt((width).W))) //数据输入
    val in_b = DeqIO(Vec(b_io_len, UInt((width).W)))
    val out_c = Output(Valid(Vec(c_io_len, UInt((width).W))))
    val work = Input(Bool())
    val stage_cycle = Input(UInt(9.W))
  })
  val reg_work = RegInit(VecInit(Seq.fill(a_io_len + b_io_len)(false.B)))
  reg_work(0) := io.work
  for(i <- 1 until (a_io_len + b_io_len)){
    reg_work(i) := reg_work(i-1)
  }
  val dfs = List((vecA, 0), (vecB, 0), (vecC, 1)).map{case (y, z)=>
    val xy_diff = y(0)!=0||y(1)!=0
    val t_diff = y(2)!=0
    var ret = StationaryInputDF
    print(xy_diff)
    print(t_diff)
    println()
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
  print("DFS:"+dataflowA+" "+dataflowB+" "+dataflowC)
  io.in_a.ready := true.B
  io.in_b.ready := true.B
  val a_stat = (dataflowA == StationaryInputDF || dataflowA == StationaryOutputDF)
  val b_stat = (dataflowB == StationaryInputDF || dataflowB == StationaryOutputDF)
  val c_stat = (dataflowC == StationaryInputDF || dataflowC == StationaryOutputDF)
  
  val pes = for(i <- 0 until pe_h) yield{
    for(j <- 0 until pe_w) yield{
      Module(new PE(1, 1, width,dataflowA, dataflowB, dataflowC)).io
    }
  }
  val cur_cycle = for(i <- 0 until pe_h) yield{
    for(j <- 0 until pe_w) yield{
      RegInit(0.U(9.W))
    }
  }
  cur_cycle(0)(0):= cur_cycle(0)(0)+io.work
  for(i <- 0 until pe_h){
    if(i!=0)
      cur_cycle(i)(0) := cur_cycle(i-1)(0)
    for(j <- 1 until pe_w){
      cur_cycle(i)(j) := cur_cycle(i)(j-1)
    }
  }
  for(j <- 0 until pe_w){
    for(i <- 1 until pe_h){
      pes(i)(j).in_a <> pes(i-1)(j).out_a
    }
    pes(0)(j).in_a.bits := io.in_a.bits(j)
    pes(0)(j).in_a.valid := reg_work(j)
  }
  // stat input
  if(vecA(0)==0&&vecA(1)==0){
    for(i <- 0 until pe_h){
      for(j <- 1 until pe_w){
        pes(i)(j).out_a <> pes(i)(j-1).in_a
      }
      pes(i)(pe_w-1).in_a.bits := io.in_a.bits(i)
      pes(i)(pe_w-1).in_a.valid := reg_work(i)
    }
  }
  if(vecA(0)==0&&vecA(1)==1){
    for(j <- 0 until pe_w){
      for(i <- 1 until pe_h){
        pes(i)(j).in_a <> pes(i-1)(j).out_a
      }
      pes(0)(j).in_a.bits := io.in_a.bits(j)
      pes(0)(j).in_a.valid := reg_work(j)
    }
  }
  if(vecA(0)==1&&vecA(1)==0){
    for(i <- 0 until pe_h){
      for(j <- 1 until pe_w){
        pes(i)(j).in_a <> pes(i)(j-1).out_a
      }
      pes(i)(0).in_a.bits := io.in_a.bits(i)
      pes(i)(0).in_a.valid := reg_work(i)
    }
  }
  if(vecA(0)==1&&vecA(1)==1){
    var in_id = 0
    for(i <- 0 until pe_h){
      for(j <- 0 until pe_w){
        if(i-1>=0&&j-1>=0){
          pes(i)(j).in_a <> pes(i-1)(j-1).out_a
        }else{
          pes(i)(j).in_a.bits := io.in_a.bits(in_id)
          pes(i)(j).in_a.valid := true.B
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
      pes(i)(pe_w-1).in_b.bits := io.in_b.bits(i)
      pes(i)(pe_w-1).in_b.valid := reg_work(i)
    }
  }
  if(vecB(0)==0&&vecB(1)==1){
    for(j <- 0 until pe_w){
      for(i <- 1 until pe_h){
        pes(i)(j).in_b <> pes(i-1)(j).out_b
      }
      pes(0)(j).in_b.bits := io.in_b.bits(j)
      pes(0)(j).in_b.valid := reg_work(j)
    }
  }
  if(vecB(0)==1&&vecB(1)==0){
    for(i <- 0 until pe_h){
      for(j <- 1 until pe_w){
        pes(i)(j).in_b <> pes(i)(j-1).out_b
      }
      pes(i)(0).in_b.bits := io.in_b.bits(i)
      pes(i)(0).in_b.valid := reg_work(i)
    }
  }
  if(vecB(0)==1&&vecB(1)==1){
    var in_id = 0
    for(i <- 0 until pe_h){
      for(j <- 0 until pe_w){
        if(i-1>=0&&j-1>=0){
          pes(i)(j).in_b <> pes(i-1)(j-1).out_b
        }else{
          pes(i)(j).in_b.bits := io.in_b.bits(in_id)
          pes(i)(j).in_b.valid := io.in_b.valid
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
      io.out_c.bits(j) := pes(pe_h-1)(j).out_c.bits
    }
  }
  //systolic, horizontal
  if(vecC(0)==1&&vecC(1)==0&&vecC(2)==1){
    for(i <- 1 until pe_h){
      for(j <- 0 until pe_w){
        pes(i)(j).in_c <> pes(i-1)(j).out_c
      }
      pes(i)(0).in_c.bits := 0.U
      pes(i)(0).in_c.valid := true.B
      io.out_c.bits(i) := pes(i)(pe_w-1).out_c.bits
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
          io.out_c.bits(out_id) := pes(i)(j).out_c.bits
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
      io.out_c.bits(i) := tree.io.out.bits
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
      io.out_c.bits(i) := tree.io.out.bits
    }
  }
  for(i <- 0 until pe_h){
    for(j <- 0 until pe_w){
      pes(i)(j).valid := reg_work(i+j)
      if(a_stat){
        pes(i)(j).a_sig_in2trans.get := (cur_cycle(i)(j)>0.U&&cur_cycle(i)(j)<=i.asUInt&&reg_work(i+j))
        pes(i)(j).a_sig_stat2trans.get := (cur_cycle(i)(j)===0.U&&reg_work(i+j))
      }
      if(b_stat){
        pes(i)(j).b_sig_in2trans.get := (cur_cycle(i)(j)>0.U&&cur_cycle(i)(j)<=i.asUInt&&reg_work(i+j))
        pes(i)(j).b_sig_stat2trans.get := (cur_cycle(i)(j)===0.U&&reg_work(i+j))
      }
      if(c_stat){
        pes(i)(j).c_sig_in2trans.get := (cur_cycle(i)(j)>0.U&&cur_cycle(i)(j)<=i.asUInt&&reg_work(i+j))
        pes(i)(j).c_sig_stat2trans.get := (cur_cycle(i)(j)===0.U&&reg_work(i+j))
      }
    }
  }
  io.out_c.valid := true.B
}
class PEArray2 extends PEArray(16, 16, 16, Array(1, 0, 1), Array(1, 1, 0), Array(1, 0, 0)){
}
class PEArray3 extends PEArray(16, 16, 16, Array(1, 1, 1), Array(0, 1, 0), Array(1, 0, 0)){
}
class PEArray4 extends PEArray(16, 16, 16, Array(1, 1, 1), Array(1, 1, 0), Array(1, 0, 0)){
}
class PEArray5 extends PEArray(16, 16, 16, Array(1, 1, 1), Array(0, 0, 1), Array(0, 1, 0)){
}
class PEArray6 extends PEArray(16, 16, 16, Array(1, 0, 1), Array(0, 1, 1), Array(0, 1, 0)){
}
class PEArray7 extends PEArray(16, 16, 16, Array(0, 0, 1), Array(1, 1, 1), Array(0, 1, 0)){
}
object Test extends App {
  //chisel3.Driver.execute(args, () => new PE(1, 1, 16, SystolicInputDF, SystolicInputDF, StationaryOutputDF))
  //chisel3.Driver.execute(args, () => new PEArrayWS(16, 16, 16, Array(1, 0, 1), Array(0, 0, 1), Array(0, 1, 1)) )
  chisel3.Driver.execute(args, () => new PEArray2() )
  chisel3.Driver.execute(args, () => new PEArray3() )
  chisel3.Driver.execute(args, () => new PEArray4() )
  chisel3.Driver.execute(args, () => new PEArray5() )
  chisel3.Driver.execute(args, () => new PEArray6() )
  chisel3.Driver.execute(args, () => new PEArray7() )
  //chisel3.Driver.execute(args, () => new PEArrayGen() )
}
// class PEArray(pe_w: Int, pe_h: Int, in_slot_num: Int, ker_slot_num: Int, cycle_read_kernel: Int, cycle_read_input: Int, cycle_out_res: Int, max_ks: Int, max_w: Int, batch: Int, width: Int) extends Module{
//   val io = IO(new Bundle{
//     val inst = DeqIO(new RoCCInstruction()) //指令输入
//     val a_in = DeqIO(Vec(cycle_read_kernel, UInt((width).W))) //数据输入
//     val b_in = DeqIO(Vec(cycle_read_input, UInt((batch*width).W)))
//     val c_out = Output(Valid(Vec(cycle_out_res, UInt((batch*width).W))))
//   })
//   val in_a_valid = RegInit(VecInit(Seq.fill(pe_h)(false.B)))
//   val in_b_valid = RegInit(VecInit(Seq.fill(pe_w)(false.B)))
//   val total_cycle = RegInit(1000.U(10.W))
//   val exec_cycle = RegInit(0.U(10.W))
//   val exec_fin = RegInit(VecInit(Seq.fill(pe_h+1)(false.B)))
//   val ids = Module(new InstDispatcher()).io
//   val a_input = Module(new WSSysIn_Kernel(pe_w, pe_h, ker_slot_num, max_ks * max_ks * pe_w, pe_w, width))
//   val b_input = Module(new WSSysIn_Input(pe_w, in_slot_num, max_w, cycle_read_input, batch * width))
//   val c_output = Module(new Update_Result(pe_h, in_slot_num, max_w, pe_h, batch*width))
//   val pes = for(i <- 0 until pe_h) yield{
//     for(j <- 0 until pe_w) yield{
//       Module(new WSPE(j, pe_w, 1, batch, width)).io
//     }
//   }
//   val part_sum = for(i <- 0 until pe_h) yield{
//     RegInit(VecInit(Seq.fill(max_w)(0.U(width.W))))
//   }
//   // 控制conv_exec指令的完成，在最后一行PE开始输出结果之后
//   exec_fin(0):= (exec_cycle===total_cycle-1.U)
//   for(i <- 1 until pe_h){
//     exec_fin(i) := exec_fin(i-1)
//   }
//   // 一开始的pe_w个cycle，输入filter到PE中，接下来的ks*ks*out_w个cycle用于计算。
//   // printf("a.inputready=%d, b.inputready=%d\n",io.a_in.ready, io.b_in.ready)
//   // printf("a.inputvalid=%d, b.inputvalid=%d\n",io.a_in.valid, io.b_in.valid)
//   // printf("total cycle=%d, exec_cycle=%d, ks=%d, w=%d, conv_exec.valid=%d\n", total_cycle, exec_cycle, ids.config.ks, ids.config.out_w, ids.conv_exec.valid)
//   printf("kernel buffer to PE\n")
//   for(i <- 0 until cycle_read_kernel){
//     printf("(%d, %d)",a_input.io.data_out.bits(i).bits,a_input.io.data_out.bits(i).valid)
//   }
//   printf("\n")
//   printf("input buffer to PE\n")
//   for(i <- 0 until cycle_read_input){
//     printf("(%d, %d)",b_input.io.data_out.bits(i).bits,b_input.io.data_out.bits(i).valid)
//   }
//   // printf("PE to output buffer\n")
//   // for(i <- 0 until cycle_read_input){
//   //   printf("(%d, %d)",c_output.io.data_in(i).bits,c_output.io.data_in(i).valid)
//   // }
//   // printf("\n")
//   total_cycle := ids.config.ks * ids.config.ks * ids.config.out_w + pe_w.asUInt + 1.U
//   ids.inst <> io.inst


//   a_input.io.in_inst <> ids.wr_filter
//   a_input.io.out_inst.bits.id := ids.conv_exec.bits.filter_id
//   a_input.io.out_inst.valid := ids.conv_exec.valid
//   a_input.io.data_in.valid := io.a_in.valid
//   for(i <- 0 until cycle_read_kernel){
//     a_input.io.data_in.bits(i).bits := io.a_in.bits(i)
//     a_input.io.data_in.bits(i).valid := io.a_in.valid
//   }
//   b_input.io.in_inst <> ids.wr_input
//   b_input.io.out_inst.bits.id := ids.conv_exec.bits.input_id
//   b_input.io.out_inst.valid := ids.conv_exec.valid
//   b_input.io.data_in.valid := io.b_in.valid
//   for(i <- 0 until cycle_read_input){
//     b_input.io.data_in.bits(i).bits := io.b_in.bits(i)
//     b_input.io.data_in.bits(i).valid := io.b_in.valid
//   }
//   c_output.io.in_inst.bits.id := ids.conv_exec.bits.output_id
//   c_output.io.in_inst.valid := ids.conv_exec.valid
//   c_output.io.out_inst <> ids.rd_output
//   io.c_out.valid := c_output.io.data_out.valid
//   ids.conv_exec.ready := exec_fin(pe_h-1) //最后一个cycle，conv指令结束，ready置为真，允许下一条指令进来
//   io.a_in.ready := a_input.io.data_in.ready
//   io.b_in.ready := b_input.io.data_in.ready
//   // filter的输入：每out_w个cycle，前pe_w个cycle输入，共输入ks*ks次
//   a_input.io.data_out.ready:=(exec_cycle%ids.config.out_w < pe_w.asUInt && exec_cycle < total_cycle - pe_w.asUInt)
//   // input的输入：前pe_w个cycle不输入，之后一直输入
//   b_input.io.data_out.ready:=(exec_cycle >= pe_w.asUInt)
//   a_input.io.config:=ids.config
//   b_input.io.config:=ids.config
//   c_output.io.config:=ids.config
//   a_input.io.data_in.valid:=io.a_in.valid
//   b_input.io.data_in.valid:=io.b_in.valid
//   io.c_out:=c_output.io.data_out

//   //如果input buffer的输出指令和filter buffer的输出指令均有效，则视为开始计算
//   //exec_cycle对应的是0号PE的cycle情况
//   exec_cycle := (exec_cycle + ids.conv_exec.valid)%total_cycle

  
//   for(i <- 0 until pe_h){
//     for(j <- 1 until pe_w){
//       pes(i)(j).out_a <> pes(i)(j-1).in_a
//       pes(i)(j).in_c <> pes(i)(j-1).out_c
//       pes(i)(j).in_stage_cycle := pes(i)(j-1).out_stage_cycle
//     }
//   }
//   for(i <- 1 until pe_h){
//     for(j <- 0 until pe_w){
//       pes(i)(j).in_b <> pes(i-1)(j).out_b
//     }
//   }
//   for(i <- 0 until pe_h){
//     for(j <- 1 until max_w){
//       part_sum(i)(j) := part_sum(i)(j-1)
//     }
//     part_sum(i)(0) := pes(i)(pe_w-1).out_c.bits
//     pes(i)(0).in_stage_cycle := ids.config.out_w
//     pes(i)(0).in_c.bits := part_sum(i)(ids.config.out_w-1.U)
//     pes(i)(0).in_c.valid := true.B
//     pes(i)(pe_w-1).in_a.bits := a_input.io.data_out.bits(i).bits
//     pes(i)(pe_w-1).in_a.valid := a_input.io.data_out.bits(i).valid
    
//     c_output.io.data_in(i).bits := part_sum(i)(pe_w-1)
//     c_output.io.data_in(i).valid := pes(i)(pe_w-1).out_c.valid
//   }
//   for(i <- 0 until pe_w){
//     pes(0)(i).in_b.bits := b_input.io.data_out.bits(i).bits
//     pes(0)(i).in_b.valid := b_input.io.data_out.bits(i).valid
//   }
//   printf("Exec cycle:%d\n",exec_cycle)
//   for(i <- 0 until pe_h){
//     for(j <- 0 until pe_w){
//       //printf("%d ",part_sum(i)(j))
//       printf("(%d %d %d) ", pes(i)(j).out_a.bits, pes(i)(j).out_b.bits, pes(i)(j).out_c.bits)
//       //printf("(a:%d, %d b:%d, %d c:%d, %d) ", pes(i)(j).out_a.bits, pes(i)(j).out_a.valid, pes(i)(j).out_b.bits,pes(i)(j).out_b.valid, pes(i)(j).out_c.bits, pes(i)(j).out_c.valid)
//     }
//     printf("\n")
    
//   }
//   printf("\n")
// }

// class WSSystolic(pe_w: Int, pe_h: Int, in_slot_num: Int, ker_slot_num: Int, cycle_read_kernel: Int, cycle_read_input: Int, cycle_out_res: Int, max_ks: Int, max_w: Int, batch: Int, width: Int) extends Module{
//   val io = IO(new Bundle{
//     val inst = DeqIO(new RoCCInstruction()) //指令输入
//     val a_in = DeqIO(Vec(cycle_read_kernel, UInt((width).W))) //数据输入
//     val b_in = DeqIO(Vec(cycle_read_input, UInt((batch*width).W)))
//     val c_out = Output(Valid(Vec(cycle_out_res, UInt((batch*width).W))))
//   })
//   // val io = IO(new Bundle{
//   //   val a_in = Vec(8, Valid(UInt(8.W)))
//   //   val b_in = Vec(8, Valid(UInt(8.W)))
//   //   val c_out = Vec(8, Valid(UInt(8.W)))
//   // })
//   // stage cycle=24, cur cycle=8
//   val in_a_valid = RegInit(VecInit(Seq.fill(8)(false.B)))
//   val in_b_valid = RegInit(VecInit(Seq.fill(8)(false.B)))
//   val exec_cycle = RegInit(0.U(10.W))
//   val ids = Module(new InstDispatcher()).io
//   val a_input = Module(new WSSysIn_Kernel(pe_w: pe_w, pe_h: pe_h, slot_num: ker_slot_num, slot_size: max_ks * max_ks * pe_w, cycle_read: Int, width: Int))
//   val b_input = Module(new WSSysIn_Input(pe_num: pe_w, slot_num: in_slot_num, slot_size: max_w * batch, cycle_read: cycle_read_input, width: Int))
//   val c_output = Module(new Update_Result(x, s, max_input_h, max_input_w/s, cycle_out_res, m*n*width))
//   val b_wait_cycle = RegInit(0.U(10.W))
//   when(ids.rd_input.ready && ids.rd_input.valid){
//     b_wait_cycle := pe_w.asUInt - 1.U
//   }.otherwise{
//     b_wait_cycle := Mux(b_wait_cycle===0.U, 0.U, b_wait_cycle - ids.rd_input.valid)
//   }
//   val a_input_cycle = RegInit(0.U(10.W))
//   when(ids.rd_filter.ready && ids.rd_filter.valid){
//     a_input_cycle := 0.U
//   }.otherwise{
//     a_input_cycle := Mux(a_input_cycle + ids.rd_filter.valid===ids.config.out_w, 0.U, a_input_cycle + ids.rd_filter.valid)
//   }
//   a_input.io.in_inst <> ids.wr_filter
//   a_input.io.out_inst <> ids.rd_filter
//   b_input.io.in_inst <> ids.wr_input
//   b_input.io.out_inst <> ids.rd_input
//   c_output.io.in_inst <> ids.wr_output
//   c_output.io.out_inst <> ids.rd_output
//   io.a_in.ready := a_input.io.data_in.ready
//   io.b_in.ready := b_input.io.data_in.ready
//   a_input.io.data_out.ready:=(a_input_cycle < pe_w)
//   b_input.io.data_out.ready:=(b_wait_cycle===0.U)
//   a_input.io.config:=ids.config
//   b_input.io.config:=ids.config
//   c_output.io.config:=ids.config
//   a_input.io.data_in.valid:=io.a_in.valid
//   b_input.io.data_in.valid:=io.b_in.valid
//   io.c_out:=c_output.io.data_out

//   //如果input buffer的输出指令和filter buffer的输出指令均有效，则视为开始计算
//   //exec_cycle对应的是0号PE的cycle情况
//   exec_cycle := (exec_cycle + ids.rd_input.valid && ids.rd_filter.valid)%ids.config.out_w
//   when(exec_cycle < input_){
//     a_input.io.data_out.ready
//   }.otherwise{
//     in_a_valid(0) := 0.U
//   }
//   for(i <- 1 until 8){
//     in_a_valid(i) := in_a_valid(i-1)
//     in_b_valid(i) := in_b_valid(i-1)
//   }
//   val pes = for(i <- 0 until 8) yield{
//     for(j <- 0 until 8) yield{
//       Module(new WSPE(j, 16, 1, 1, 16)).io
//     }
//   }
//   for(i <- 0 until 8){
//     for(j <- 1 until 8){
//       pes(i)(j).in_a <> pes(i)(j-1).out_a
//       pes(i)(j).in_c <> pes(i)(j-1).out_c
//       pes(i)(j).in_stage_cycle := pes(i)(j-1).out_stage_cycle
//     }
//   }
//   for(i <- 1 until 8){
//     for(j <- 0 until 8){
//       pes(i)(j).in_b <> pes(i-1)(j).out_b
//     }
//   }
//   for(i <- 0 until 8){
//     pes(i)(0).in_stage_cycle := 24.U
//     pes(i)(0).in_c.bits := 0.U
//     pes(i)(0).in_c.valid := true.B
//     pes(i)(0).in_a.bits := cur_data
//     pes(i)(0).in_a.valid := in_a_valid(i)
//   }
//   for(i <- 0 until 8){
//     pes(0)(i).in_b.bits := 1.U
//     pes(0)(i).in_b.valid := in_b_valid(i)
//   }
//   for(i <- 0 until 8){
//     for(j <- 0 until 8){
//       printf("(%d %d %d) ", pes(i)(j).out_a.bits, pes(i)(j).out_b.bits, pes(i)(j).out_c.bits)
//       //printf("(a:%d, %d b:%d, %d c:%d, %d) ", pes(i)(j).out_a.bits, pes(i)(j).out_a.valid, pes(i)(j).out_b.bits,pes(i)(j).out_b.valid, pes(i)(j).out_c.bits, pes(i)(j).out_c.valid)
//     }
//     printf("\n")
    
//   }
//   printf("\n")
// }

// class WSPE_BitFusion(id: Int, dim: Int, m: Int, n: Int, width: Int) extends Module{
//     val io = IO(new Bundle {
//       val in_stage_cycle = Input(UInt(10.W))
//       val out_stage_cycle = Output(UInt(10.W))
//       val in_a = Input(Valid(UInt((m*width).W)))
//       val in_b = Input(Valid(UInt((n*width).W)))
//       val in_c = Input(Valid(UInt(128.W)))
//       val out_c = Output(Valid(UInt(128.W)))
//       val out_a = Output(Valid(UInt((m*width).W)))
//       val out_b = Output(Valid(UInt((n*width).W)))
//     })
//     val stage_cycle = RegInit(1.U(10.W))  
//     val reg_b = RegInit(0.U.asTypeOf(Valid(UInt((n*width).W))))
//     val reg_c = RegInit(0.U.asTypeOf(Valid(UInt((128).W))))
//     val pe = Module(new DynamicPE_WS()).io
    

//     val trans_a = RegInit(0.U.asTypeOf(Valid(UInt((m*width).W))))
//     val stat_a = RegInit(0.U.asTypeOf(Valid(UInt((m*width).W))))
//     val exec_cycle = RegInit(0.U(10.W))
//     val input_cycle = RegInit(0.U(10.W))

//     pe.ctrl := 4.U
//     pe.sgn := 1.U
//     pe.statC_in := io.in_c.bits
//     reg_c.bits := pe.statC_out
//     pe.in_row := stat_a.bits
//     pe.in_column := Mux(reg_b.valid, reg_b.bits, 0.U)
//     //val stat = Module(new StatIn(id, dim, m, n, width)).io
//     stage_cycle := io.in_stage_cycle
//     io.out_stage_cycle := stage_cycle

//     exec_cycle:=Mux(exec_cycle+(io.in_b.valid)===stage_cycle, 0.U, exec_cycle+(io.in_b.valid))
    

//     // 对于input，照单全收，由整体的controller控制
//     reg_b.bits := io.in_b.bits
//     reg_b.valid := io.in_b.valid
//     trans_a.bits := io.in_a.bits
//     trans_a.valid := io.in_a.valid
//     when(io.in_a.valid){
//       input_cycle := Mux(input_cycle===(dim-1).asUInt,0.U, input_cycle+1.U)
//     }

//     when(exec_cycle===0.U){
//       stat_a := trans_a
//     }
//     //io.out_a := stat.to_io
//     // pe.in_a := stat_a.bits
//     // when(reg_b.valid){
//     //   pe.in_b := reg_b.bits
//     // }.otherwise{
//     //   pe.in_b := 0.U
//     // }
//     reg_c.valid := io.in_b.valid
//     io.out_c.bits := reg_c.bits
//     io.out_c.valid := reg_c.valid
//     //pe.in_c := io.in_c.bits
//     io.out_b.bits := reg_b.bits
//     io.out_b.valid := reg_b.valid
//     io.out_a.bits := trans_a.bits
//     io.out_a.valid := trans_a.valid
// }
// class WSSystolic(s: Int, x: Int, max_input_w: Int, max_input_h: Int, max_c: Int, max_ks: Int, cycle_read_input: Int, cycle_read_kernel: Int, cycle_out_res: Int, m: Int, n: Int, width: Int) extends Module{
//   val io = IO(new Bundle{
//     val inst = DeqIO(new RoCCInstruction()) //指令输入
//     //val config = Input(new ConvConfig())  //后续会合并到inst里
//     val a_in = DeqIO(Vec(cycle_read_kernel, UInt((n*width).W))) //数据输入
//     val b_in = DeqIO(Vec(cycle_read_input, UInt((m*width).W)))
//     val c_out = Output(Valid(Vec(cycle_out_res, UInt((m*n*width).W))))
//   })
  
//   //assert(s*x/t==1)
  
//   val ids = Module(new InstDispatcher()).io
//   printf("config: %d %d %d %d\n",ids.config.c, ids.config.ks, ids.config.in_w, ids.config.in_h)
//   val stage_cycle = RegInit(333333.U(20.W))
//   stage_cycle := ids.config.in_w

//   //ids.config := io.config
//   val pes = for(i <- 0 until s) yield{
//     for(j <- 0 until x) yield{
//       Module(new WSPE(i, s, m, n, width))
//     }
//   }
//   // printf("PE status\n")
//   //   for(j <- 0 until x){
//   //     printf("(%d, %d, %d, %d)",pes(0)(j).io.in_b.bits,pes(0)(j).io.in_b.valid, pes(0)(j).io.cur_dt, pes(0)(j).io.cur_cycle)
//   //   }
//   // printf("\n")
//   val a_input = Module(new DFSysIn_Kernel(x, s, max_c*max_ks*max_ks, 3,cycle_read_kernel, n*width))
//   val b_input = Module(new DFSysIn_Input(x, max_input_w, max_c, max_ks, cycle_read_input, m*width))
//   val c_output = Module(new Update_Result(x, s, max_input_h, max_input_w/s, cycle_out_res, m*n*width))
//   //val transC = Module(new DCStatOut(t, s, x, m*n*width))
//   a_input.io.in_inst <> ids.wr_filter
//   a_input.io.out_inst <> ids.rd_filter
//   b_input.io.in_inst <> ids.wr_input
//   b_input.io.out_inst <> ids.rd_input
//   c_output.io.in_inst <> ids.wr_output
//   c_output.io.out_inst <> ids.rd_output
//   io.a_in.ready := a_input.io.data_in.ready
//   io.b_in.ready := b_input.io.data_in.ready
//   a_input.io.data_out.ready:=true.B
//   b_input.io.data_out.ready:=true.B
//   a_input.io.config:=ids.config
//   b_input.io.config:=ids.config
//   c_output.io.config:=ids.config
//   a_input.io.data_in.valid:=io.a_in.valid
//   b_input.io.data_in.valid:=io.b_in.valid
//   io.c_out:=c_output.io.data_out
//   for(i <- 0 until cycle_read_kernel){
//     a_input.io.data_in.bits(i):=io.a_in.bits(i).asUInt
//   }
//   for(i <- 0 until cycle_read_input){
//     b_input.io.data_in.bits(i):=io.b_in.bits(i).asUInt
//   }
//   for(i <- 0 until s){
//     pes(i)(0).io.in_stage_cycle:=reduce_cycle
//     pes(i)(0).io.in_a:=a_input.io.data_out.bits(i)
//   }
//   for(i<- 0 until s){
//     for(j<- 1 until x){
//       pes(i)(j).io.in_stage_cycle:=pes(i)(j-1).io.out_stage_cycle
//       pes(i)(j).io.in_a:=pes(i)(j-1).io.out_a
//     }
//   }
//   for(i <- 0 until x){
//     pes(0)(i).io.in_b:=b_input.io.data_out.bits(i)
//   }
//   for(i<- 0 until x){
//     for(j<- 1 until s){
//       pes(j)(i).io.in_b:=pes(j-1)(i).io.out_b
//     }
//   }
//   for(i <- 0 until x){
//     pes(0)(i).io.in_c.bits:=0.U
//     pes(0)(i).io.in_c.valid:=false.B
//   }
//   for(i <- 1 until s){
//     for(j <- 0 until x){
//       pes(i)(j).io.in_c:=pes(i-1)(j).io.out_c
//     }
//   }
//   for(i <- 0 until x){
//     c_output.io.data_in(i):=pes(s-1)(i).io.out_c
//   }
//   //   //io.c_out(i).valid:=pes(s-1)(i).io.res_out.valid
//   // }
//   // for(i <- 0 until s*x/t){
//   //   io.c_out(i).valid:=transC.io.data_out.valid
//   //   io.c_out(i).bits:=transC.io.data_out.bits(i)
//   // }
//   // transC.io.data_out.ready:=true.B
// }