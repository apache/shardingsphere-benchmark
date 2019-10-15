<template>
  <Card class="card-wrap">
    <p slot="title">
      <Icon type="ios-analytics" />&nbsp;Overview
    </p>
    <Row :gutter="16">
      <Col span="6">
        <Card class="card-item">
          <router-link to="mysql-vs-sharding">
            <div class="card-item-left left01">01</div>
            <h3>mysql vs sharding</h3>
            <span class="card-span">detail</span>
          </router-link>
        </Card>
      </Col>
      <Col span="6">
        <Card class="card-item">
          <router-link to="sharding-proxy-master-slave">
            <div class="card-item-left left02">02</div>
            <h3>sharding proxy master slave</h3>
            <span class="card-span">detail</span>
          </router-link>
        </Card>
      </Col>
      <Col span="6">
        <Card class="card-item">
          <router-link to="sharding-proxy-master-slave-sharding">
            <div class="card-item-left left03">03</div>
            <h3>sharding proxy master slave sharding</h3>
            <span class="card-span">detail</span>
          </router-link>
        </Card>
      </Col>
      <Col span="6">
        <Card class="card-item">
          <router-link to="sharding-proxy-single-database-single-table">
            <div class="card-item-left left04">04</div>
            <h3>sharding proxy single database single table</h3>
            <span class="card-span">detail</span>
          </router-link>
        </Card>
      </Col>
      <Col span="6">
        <Card class="card-item">
          <router-link to="shardingjdbc-vs-shardingproxy-encrypt">
            <div class="card-item-left left05">05</div>
            <h3>shardingjdbc vs shardingproxy encrypt</h3>
            <span class="card-span">detail</span>
          </router-link>
        </Card>
      </Col>
      <Col span="6">
        <Card class="card-item">
          <router-link to="shardingjdbc-vs-shardingproxy-sharding-encrypt">
            <div class="card-item-left left06">06</div>
            <h3>shardingjdbc vs shardingproxy sharding encrypt</h3>
            <span class="card-span">detail</span>
          </router-link>
        </Card>
      </Col>
    </Row>
  </Card>
</template>
<script>
export default {
  name: 'Overview'
}
</script>
<style scoped>
.card-wrap {
  margin: 20px;
  height: calc(100vh - 165px);
}
.card-item {
  min-height: 150px;
  position: relative;
  margin-bottom: 15px;
}
h3 {
  color: #333;
}
.card-span {
  position: absolute;
  right: 10px;
  bottom: 10px;
}
.card-item-left {
  width: 50px;
  height: 50px;
  float: left;
  line-height: 50px;
  text-align: center;
  border-radius: 50px;
  margin-right: 10px;
  font-size: 18px;
  font-weight: bold;
  color: #fff;
}
.left01 {
  background: #1ba3f7;
}
.left02 {
  background: #d08553;
}
.left03 {
  background: #fd9927;
}
.left04 {
  background: #a177ca;
}
.left05 {
  background: #00b0cf;
}
.left06 {
  background: #8fa6ee;
}
</style>