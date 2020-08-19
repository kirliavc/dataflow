package systolic

import chisel3._
import chisel3.util._
import scala.math.log10
import chisel3.stage.ChiselStage
//import chisel3.Driver
import chisel3.iotesters.{PeekPokeTester, Driver}
import java.io.PrintWriter


import TensorDataflow._
class MttkrpPE(m: Int, n: Int, width: Int, dataflowA: TensorDataflow, dataflowB: TensorDataflow, dataflowC: TensorDataflow) extends Module{
  //print(m+" "+n+" "+width+" "+dataflowA+" "+dataflowB+" "+dataflowC)
  val a_stat = (dataflowA == StationaryInputDF || dataflowA == StationaryOutputDF)
  val b_stat = (dataflowB == StationaryInputDF || dataflowB == StationaryOutputDF)
  val c_stat = (dataflowC == StationaryInputDF || dataflowC == StationaryOutputDF)
  val io = IO(new Bundle {
    val in_a = Input(Valid(UInt((m*width).W)))
    val in_b = Input(Valid(UInt((2*n*width).W)))
    val in_c = Input(Valid(UInt((m*n*width).W)))
    val out_c = Output(Valid(UInt((m*n*width).W)))
    val out_a = Output(Valid(UInt((m*width).W)))
    val out_b = Output(Valid(UInt((2*n*width).W)))
    val a_sig_in2trans = if(a_stat)Some(Input(Bool()))else None
    val b_sig_in2trans = if(b_stat)Some(Input(Bool()))else None
    val c_sig_in2trans = if(c_stat)Some(Input(Bool()))else None
    val a_sig_stat2trans = if(a_stat)Some(Input(Bool()))else None
    val b_sig_stat2trans = if(b_stat)Some(Input(Bool()))else None
    val c_sig_stat2trans = if(c_stat)Some(Input(Bool()))else None
  })
  val pe = Module(new ComputeCell_Mttkrp(m, n, width)).io
  List((io.in_a, io.out_a, pe.in_a, null,io.a_sig_in2trans, io.a_sig_stat2trans, dataflowA, m), (io.in_b, io.out_b, pe.in_b, null, io.b_sig_in2trans, io.b_sig_stat2trans, dataflowB, 2*n), ( io.in_c, io.out_c, pe.in_c, pe.out_c, io.c_sig_in2trans, io.c_sig_stat2trans, dataflowC, m*n)).map{ case (in, out, pein, peout, sig1, sig2, df, vect)=>
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

class MttkrpPEArray(pe_h: Int, pe_w: Int, width: Int, vecA: Array[Int], vecB: Array[Int], vecC: Array[Int], veca: Int, vecb: Int) extends Module{
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
    val in_b = Input(Vec(b_io_len, Valid(UInt((2*vecb*width).W))))
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
      Module(new MttkrpPE(veca, vecb, width,dataflowA, dataflowB, dataflowC)).io
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
class PEArrayMem_Mttkrp(pe_h: Int, pe_w: Int, width: Int, vecA: Array[Int], vecB: Array[Int], vecC: Array[Int], veca: Int, vecb: Int) extends Module{
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
  val pearray = Module(new MttkrpPEArray(pe_h, pe_w, width, vecA, vecB, vecC, veca, vecb)).io
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

object Test_Mttkrp extends App {
  //chisel3.Driver.execute(args, () => new PE(1, 1, 16, SystolicInputDF, SystolicInputDF, StationaryOutputDF))
  //chisel3.Driver.execute(args, () => new PEArrayWS(16, 16, 16, Array(1, 0, 1), Array(0, 0, 1), Array(0, 1, 1)) )
  var dfid = 0
  var mat = Array(Array(1,0,1),Array(0,1,1),Array(0,0,1))
  
  chisel3.Driver.execute(args, () => new PEArrayMem_Mttkrp(16,8,16,mat(0),mat(1),mat(2),16, 1))
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
