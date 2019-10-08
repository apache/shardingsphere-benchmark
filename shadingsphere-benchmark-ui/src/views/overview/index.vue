<template>
  <Card class="card-wrap">
    <p slot="title">
      <Icon type="ios-analytics" />Overview
    </p>
    <Row :gutter="16">
      <Col span="6">
        <Card class="card-item">
          <router-link to="mysql-vs-sharding">
            <h3>mysql vs sharding</h3>
            <span class="card-span">detail</span>
          </router-link>
        </Card>
      </Col>
      <Col span="6">
        <Card class="card-item">
          <router-link to="sharding-proxy-master-slave">
            <h3>sharding proxy master slave</h3>
            <span class="card-span">detail</span>
          </router-link>
        </Card>
      </Col>
      <Col span="6">
        <Card class="card-item">
          <router-link to="sharding-proxy-master-slave-sharding">
            <h3>sharding proxy master slave sharding</h3>
            <span class="card-span">detail</span>
          </router-link>
        </Card>
      </Col>
      <Col span="6">
        <Card class="card-item">
          <router-link to="sharding-proxy-single-database-single-table">
            <h3>sharding proxy single database single table</h3>
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
}
h3 {
  color: #333;
}
.card-span {
  position: absolute;
  right: 10px;
  bottom: 10px;
}
</style>