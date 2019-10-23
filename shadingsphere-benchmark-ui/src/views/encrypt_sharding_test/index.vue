<template>
  <Row :gutter="16" style="background:#eee; padding:20px">
    <Col
      v-for="(value, name, index) in series"
      :key="index"
      :xs="24"
      :sm="24"
      :md="24"
      :lg="24"
      :xl="24"
      :xxl="12"
      class="col-item"
    >
      <Card :bordered="false">
        <p slot="title" style="font-size: 18px">{{ name.toLowerCase() }}</p>
        <v-chart :options="getOptions(name)" />
        <Table
          :columns="columns"
          :data="[desc[name]]"
          size="large"
          style="margin-top: 20px;"
          border
        />
      </Card>
    </Col>
    <div v-show="loading" class="spin-container">
      <Spin fix>
        <Icon type="ios-loading" size="18" class="spin-icon-load" />
        <div>Loading</div>
      </Spin>
    </div>
  </Row>
</template>

<script>
import ECharts from 'vue-echarts'
import 'echarts/lib/chart/line'
import 'echarts/lib/component/tooltip'
import 'echarts/lib/component/legend'
import 'echarts/lib/component/title'
import apis from '../../utils/utils'
import { mountedMixin } from '../../utils/mixin'
import { getLineOptions } from '../../utils/line'

export default {
  name: 'EncryptShardingTest',
  components: {
    'v-chart': ECharts
  },
  mixins: [mountedMixin],
  data() {
    return {
      data: []
    }
  },
  mounted() {
    this.loading = true
    apis.getEncryptShardingTestData().then(res => {
      this.formatData(res)
      this.loading = false
    })
  },
  methods: {
    getOptions(name) {
      return getLineOptions(name, this.xAxis, this.legend, this.series)
    }
  }
}
</script>
<style>
.echarts {
  width: 100%;
  height: 400px;
}
.col-item {
  margin-bottom: 16px;
}
.ivu-form-item {
  margin-bottom: 10px !important;
}
.ivu-form-item-label {
  font-size: 15px !important;
}
.ivu-form-item-content {
  font-size: 14px !important;
}
</style>
